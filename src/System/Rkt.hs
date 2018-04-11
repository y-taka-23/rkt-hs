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

-- Todo: stub
data App = App deriving (Eq, Show)
data Network = Network deriving (Eq, Show)
