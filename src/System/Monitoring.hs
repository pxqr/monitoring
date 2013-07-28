{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module System.Monitoring
       ( -- * Server
         monitoring
       , Monitor(..), Config(..)

         -- * Events
       , signalEvent
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
import Data.Version (Version(..), showVersion)

import Network
import Network.WebSockets
import Yesod hiding (Update, Request)
import Yesod.Static

-- import Paths_monitoring (version)
-- linker error again :(
version :: Version
version = Version { versionBranch = [0, 1, 0, 0]
                  , versionTags = [] }

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
eventServer m @ Monitor {..} = runServer "0" port (app m)
  where
    port = fromIntegral (websockPort config)

data Config = Config
  { websockPort :: PortNumber
  } deriving (Show, Eq, Ord)

data Monitor = Monitor
  { config    :: Config
  , signal    :: EventStream
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
  Monitor { config = Config{..} } <- getYesod
  addScript     $ StaticR js_listener_js
  addScript     $ StaticR js_utils_js
  setTitle "monitoring"
  toWidgetHead styleW
  toWidgetBody $ homeW (fromIntegral websockPort :: Int)


--styleW :: Html
styleW = [hamlet|
<link rel="icon" href=@{StaticR img_favicon_png} type="image/png">
<link rel="stylesheet" type="text/css" title="ocean"
      href=@{StaticR css_ocean_css}>
<link rel="alternate stylesheet" type="text/css" title="charcoal"
      href=@{StaticR css_charcoal_css}>
|]

--homeW :: PortNumber -> RepHtml
homeW port = [hamlet|
<body onload="loadPage(#{port})">
  <div id="header">
    Style
    <a href="#" onclick="setStyle('ocean')">    Ocean
    <a href="#" onclick="setStyle('charcoal')"> Charcoal
  <div id="monitor">
  <div id="footer">
    <p> Produced by
      <a href="http://www.yesodweb.com"> Yesod
      and
      <a href="https://github.com/pxqr/monitoring"> monitoring
      version #{showVersion version}
|]

newMonitor :: Config -> IO Monitor
newMonitor config = Monitor config
  <$> newBroadcastTChanIO
  <*> newTVarIO HM.empty
  <*> static STATIC_DIR

monitoring :: PortNumber -> Config -> IO Monitor
monitoring port config = do
  m <- newMonitor config
  forkIO $ eventServer m
  forkIO $ warp (fromIntegral port) m
  return m