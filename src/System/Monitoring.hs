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
import Data.HashMap.Strict as HM
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

type EventStream = TChan Event

waitForEvent :: EventStream -> IO Event
waitForEvent = atomically . readTChan

signalEvent :: Monitor -> Event -> IO ()
signalEvent Monitor {..} (Update cid val)
  = atomically $ do
      writeTChan signal (Update cid val)
      modifyTVar' cached $ HM.insert cid val

sendEvent :: Event -> WebSockets Hybi00 ()
sendEvent = send . DataMessage . Text . encode

app :: Monitor -> Request -> WebSockets Hybi00 ()
app Monitor {..} req = do
  acceptRequest req
  (ch', cur) <- liftIO $ atomically $
       (,) <$> dupTChan signal <*> readTVar cached
  mapM_ (sendEvent . uncurry Update) (toList cur)
  forever $ sendEvent =<< liftIO (waitForEvent ch')

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