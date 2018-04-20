{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module System.Rkt where

import           Control.Monad.Reader
import           Data.IP
import           Data.Text
import           Data.Time

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
    } deriving (Eq, Show)

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
    deriving (Eq, Show)

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
    } deriving (Eq, Show)

data AppState =
      AppUnkown
    | AppCreated
    | AppRunning
    | AppExited
    deriving (Eq, Show)

data Mount = Mount {
      mountName          :: Text
    , mountContainerPath :: FilePath
    , mountHostPath      :: FilePath
    , mountReadOnly      :: Bool
    } deriving (Eq, Show)

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
    } deriving (Eq, Show)

newtype RktT m a = RktT {
      unRktT :: (Monad m) => ReaderT (GlobalOpts) m a
    } deriving (Functor)

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
listPods = undefined

runPod :: (MonadIO io) => Image -> RktT io (Either RktError UUID)
runPod = undefined

stopPod :: (MonadIO io) => UUID -> RktT io (Either RktError ())
stopPod = undefined

rmPod :: (MonadIO io) => UUID -> RktT io (Either RktError ())
rmPod = undefined

-- Todo: stub
data IPConfig = IPConfig deriving (Eq, Show)
data DNS = DNS deriving (Eq, Show)
data GlobalOpts = GlobalOpts
data RktError = RkrError
