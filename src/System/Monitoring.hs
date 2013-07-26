{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module System.Monitoring
       ( monitoring
       , signalEvent
       , Monitor(..)
       , Event(..)
       , EventStream
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Data.Text

import Network
import Network.WebSockets
import Yesod hiding (Update, Request)
import Yesod.Static

type GroupId = Text
type CounterId = Text

data Event = Update GroupId Object
           | Remove GroupId
              deriving Show

$(deriveJSON id ''Event)

instance WebSocketsData Event where
  toLazyByteString   = encode
  fromLazyByteString = error "event not receivable"

eventGroup :: Event -> GroupId
eventGroup (Update gid _) = gid
eventGroup (Remove gid)   = gid

data Subscription = Subscribe   GroupId
                  | Unsubscribe GroupId
                    deriving Show

$(deriveJSON id ''Subscription)

instance WebSocketsData Subscription where
  toLazyByteString   = encode
  fromLazyByteString = fromJust . decode

type EventStream = TChan Event

waitForEvent :: EventStream -> IO Event
waitForEvent = atomically . readTChan

signalEvent :: Monitor -> Event -> IO ()
signalEvent Monitor {..} (Update cid val)
  = atomically $ do
      writeTChan signal (Update cid val)
      modifyTVar' cached $ HM.insert cid val

type BlackList = TVar (HashSet GroupId)

notifier :: TextProtocol p => Sink p -> EventStream -> BlackList -> IO ()
notifier sink stream blackList = forever $ do
  ev <- waitForEvent stream
  bl <- readTVarIO blackList
  unless (eventGroup ev `HS.member` bl) $ do
    sendSink sink $ textData ev

subscriber :: BlackList -> WebSockets Hybi00 ()
subscriber blackList = forever $ do
  sub <- receiveData
  liftIO $ atomically $ modifyTVar' blackList $ case sub of
    Subscribe   gid -> HS.delete gid
    Unsubscribe gid -> HS.insert gid

app :: Monitor -> Request -> WebSockets Hybi00 ()
app Monitor {..} req = do
  acceptRequest req
  (ch', cur) <- liftIO $ atomically $
       (,) <$> dupTChan signal <*> readTVar cached

  mapM_ (sendTextData . uncurry Update) (HM.toList cur)

  sink      <- getSink
  blackList <- liftIO $ newTVarIO HS.empty
  liftIO $ forkIO $ notifier sink ch' blackList
  subscriber blackList


eventServer :: Monitor -> IO ()
eventServer m = runServer "0" 4000 (app m)


data Monitor = Monitor
  { signal    :: EventStream
  , cached    :: TVar (HashMap GroupId Object)
  , getStatic :: Static
  }

instance Yesod Monitor

staticFiles STATIC_DIR

mkYesod "Monitor" [parseRoutes|
/static StaticR Static getStatic
/       HomeR   GET
|]

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
  addStylesheet $ StaticR style_css
  addScript     $ StaticR listener_js
  setTitle "main"
  homeW

homeW :: Widget
homeW = [whamlet|
<body onload="listenEvents(4000)">
  <div id="monitor">
|]

newMonitor :: IO Monitor
newMonitor = Monitor <$> newTChanIO
                     <*> newTVarIO HM.empty
                     <*> static STATIC_DIR


monitoring :: PortNumber -> IO Monitor
monitoring port = do
  m <- newMonitor
  forkIO $ eventServer m
  forkIO $ warp (fromIntegral port) m
  return m