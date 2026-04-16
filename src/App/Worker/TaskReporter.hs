module App.Worker.TaskReporter
  ( startReportListener
  , initReportQueue
  , reportTaskKey
  ) where

import Effectful
import Effectful.Tasks
import NSO.Prelude
import Network.AMQP.Worker (Connection, Key, Route, key, word)


reportTaskKey :: Key Route ReportedTask
reportTaskKey = key "status" & word "level2" & word "m"


initReportQueue :: (IOE :> es) => Connection -> Eff es (QueueAMQP ReportedTask)
initReportQueue = initQueueAMQP reportTaskKey
