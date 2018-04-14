module System.Rkt where

import Data.Text
import Data.Time

data Pod = Pod {
      podUUID            :: Text
    , podState           :: PodState
    , podNetworks        :: [Network]
    , podAppNames        :: [Text]
    , podApps            :: [App]
    , podCreatedAt       :: UTCTime
    , podStartedAt       :: UTCTime
    , podUserAnnotations :: [(Text, Text)]
    , podUserLabels      :: [(Text, Text)]
    } deriving (Eq, Show)

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
    , appImageID         :: Text
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

-- Todo: stub
data Network = Network deriving (Eq, Show)
data Mount = Mount deriving (Eq, Show)
