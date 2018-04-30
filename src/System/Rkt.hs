{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module System.Rkt where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.IP
import           Data.Text
import           Data.Text.Lazy.Encoding
import           Data.Time
import           GHC.Generics
import qualified Turtle                  as T

data Pod = Pod {
      podUUID            :: UUID
    , podState           :: PodState
    , podNetworks        :: [Network]
    , podAppNames        :: [Text]
    , podApps            :: [App]
    , podCreatedAt       :: UTCTime
    , podStartedAt       :: UTCTime
    , podUserAnnotations :: [(Text, Text)]
    , podUserLabels      :: [(Text, Text)]
    } deriving (Eq, Show, Generic)

instance FromJSON Pod

-- Todo: reconsider how to fetch an UUID by --uuid-file option
type UUID = Text
-- Todo: reconsider how to deal image names
type Image = Text

data PodState =
      PodEmbryo
    | PodPreparing
    | PodAbortedPrepare
    | PodPrepared
    | PodRunning
    | PodDeleting
    | PodExitedDeleting
    | PodExited
    | PodExitedGarbage
    | PodGarbage
    deriving (Eq, Show, Generic)

instance FromJSON PodState

data App = App {
      appName            :: Text
    , appState           :: AppState
    , appCreatedAt       :: UTCTime
    , appStatedAt        :: UTCTime
    , appFinishedAt      :: UTCTime
    , appExitCode        :: Int
    , appImageID         :: Text -- Todo: replaceable by 'Image'?
    , appMounts          :: [Mount]
    , appUserAnnotations :: [(Text, Text)] -- Todo: Data.Map is better?
    , appUserLabels      :: [(Text, Text)]
    } deriving (Eq, Show, Generic)

instance FromJSON App

data AppState =
      AppUnkown
    | AppCreated
    | AppRunning
    | AppExited
    deriving (Eq, Show, Generic)

instance FromJSON AppState

data Mount = Mount {
      mountName          :: Text
    , mountContainerPath :: FilePath
    , mountHostPath      :: FilePath
    , mountReadOnly      :: Bool
    } deriving (Eq, Show, Generic)

instance FromJSON Mount

data Network = Network {
      networkNetName    :: Text
    , networkConfPath   :: FilePath
    , networkPluginPath :: FilePath
    , networkIfName     :: Text
    , networkIP         :: IPv4
    , networkArgs       :: Text  -- Todo: String or [String] is better?
    , networkMask       :: AddrRange IPv4
    , networkHostIP     :: IPv4
    , networkIP4        :: IPConfig
    , networkDNS        :: DNS
    } deriving (Eq, Show, Generic)

instance FromJSON Network

newtype RktT m a = RktT {
      unRktT :: (Monad m) => ReaderT GlobalOpts m a
    } deriving (Functor)

runRktT :: (Monad m) => GlobalOpts -> RktT m a -> m a
runRktT opts r = runReaderT (unRktT r) opts

instance (Applicative m) => Applicative (RktT m) where
    pure x = RktT $ pure x
    RktT f <*> RktT v = RktT $ f <*> v

instance (Monad m) => Monad (RktT m) where
    return = pure
    RktT m >>= f = RktT $ m >>= unRktT . f

instance (Monad m) => MonadReader GlobalOpts (RktT m) where
    ask = RktT ask
    local f (RktT m) = RktT $ local f m

instance MonadTrans RktT where
    lift m = RktT $ lift m

instance MonadIO m => MonadIO (RktT m) where
    liftIO = lift . liftIO

listPods :: (MonadIO io) => RktT io (Either RktError [Pod])
listPods = do
    let cmd = T.format ("rkt list --format json")
    (exitCode, pods) <- T.shellStrict cmd T.empty
    case exitCode of
        T.ExitSuccess -> do
            -- Todo: retrieve the parse error message
            case decode' (encodeUtf8 pods) of
                Nothing   -> return $ Left $ RktError 1
                Just pods -> return $ Right pods
        T.ExitFailure c -> return $ Left $ RktError c

runPod :: (MonadIO io) => Image -> RktT io (Either RktError UUID)
runPod = undefined

stopPod :: (MonadIO io) => UUID -> RktT io (Either RktError ())
stopPod uuid = do
    let cmd = T.format ("sudo rkt stop "T.%T.s) uuid
    (exitCode, _) <- T.shellStrict cmd T.empty
    case exitCode of
        T.ExitSuccess   -> return $ Right ()
        T.ExitFailure c -> return $ Left $ RktError c

removePod :: (MonadIO io) => UUID -> RktT io (Either RktError ())
removePod uuid = do
    let cmd = T.format ("sudo rkt rm "T.%T.s) uuid
    -- Todo: how to handle stderr?
    (exitCode, _) <- T.shellStrict cmd T.empty
    case exitCode of
        T.ExitSuccess   -> return $ Right ()
        T.ExitFailure c -> return $ Left $ RktError c

data RktError = RktError Int

-- Todo: stub
data IPConfig = IPConfig deriving (Eq, Show, Generic)
data DNS = DNS deriving (Eq, Show, Generic)
data GlobalOpts = GlobalOpts

instance FromJSON (AddrRange IPv4) where
    parseJSON = undefined

instance FromJSON IPv4 where
    parseJSON = undefined

instance FromJSON IPConfig
instance FromJSON DNS
