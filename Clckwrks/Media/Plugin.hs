{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.Media.Plugin where

import Clckwrks
import Clckwrks.IOThread         (IOThread(..), startIOThread, killIOThread)
import Clckwrks.Plugin           (clckPlugin)
import Clckwrks.Media.Acid       (initialMediaState)
import Clckwrks.Media.Monad      (MediaConfig(..), runMediaT)
import Clckwrks.Media.PreProcess (mediaCmd)
import Clckwrks.Media.Preview    (applyTransforms)
import Clckwrks.Media.Route      (routeMedia)
import Clckwrks.Media.URL        (MediaURL(..))
import Control.Concurrent        (ThreadId, killThread)
import Data.Acid                 as Acid
import Data.Acid.Local           (createCheckpointAndClose, openLocalStateFrom)
import Data.Text                 (Text)
import qualified Data.Text.Lazy  as TL
import Data.Maybe                (fromMaybe)
import Data.Set                  (Set)
import Magic                     (Magic, MagicFlag(..), magicLoadDefault, magicOpen)
import System.Directory          (createDirectoryIfMissing)
import System.FilePath           ((</>))
import Web.Plugins.Core          (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, initPlugin, getConfig, getPluginRouteFn)

mediaHandler :: (MediaURL -> [(Text, Maybe Text)] -> Text)
              -> MediaConfig
              -> ClckPlugins
              -> [Text]
              -> ClckT ClckURL (ServerPartT IO) Response
mediaHandler showMediaURL mediaConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runMediaT mediaConfig $ routeMedia u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (MediaURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showMediaURL u p

mediaInit :: ClckPlugins
           -> IO (Maybe Text)
mediaInit plugins =
    do (Just mediaShowFn) <- getPluginRouteFn plugins (pluginName mediaPlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
           mediaDir = maybe "_media" (\td -> td </> "_media") mTopDir
           cacheDir = mediaDir </> "_cache"
       createDirectoryIfMissing True cacheDir

       acid <- openLocalStateFrom (basePath </> "media") initialMediaState
       addCleanup plugins Always (createCheckpointAndClose acid)

       ioThread <- startIOThread (applyTransforms mediaDir cacheDir)
       addCleanup plugins Always (killIOThread ioThread)

       magic <- magicOpen [MagicMime, MagicError]
       magicLoadDefault magic

       let mediaConfig = MediaConfig { mediaDirectory = mediaDir
                                     , mediaState     = acid
                                     , mediaMagic     = magic
                                     , mediaIOThread  = ioThread
                                     , mediaClckURL   = clckShowFn
                                     }

       addPreProc plugins (mediaCmd mediaShowFn)
       addHandler plugins (pluginName mediaPlugin) (mediaHandler mediaShowFn mediaConfig)
       return Nothing

mediaPlugin :: Plugin MediaURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig [TL.Text -> ClckT ClckURL IO TL.Text]
mediaPlugin = Plugin
    { pluginName       = "media"
    , pluginInit       = mediaInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return ()
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI mediaPlugin
