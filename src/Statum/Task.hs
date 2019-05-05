module Statum.Task
    ( Task(..)
    ) where


import qualified Statum.Task.DiskSpacePoller as DiskSpacePoller
import qualified Statum.Task.InterfacePoller as InterfacePoller
import qualified Statum.Task.MemInfoPoller as MemInfoPoller
import qualified Statum.Task.StatPoller as StatPoller


data Task
    = InterfacePoller InterfacePoller.Config
    | StatPoller StatPoller.Config
    | MemInfoPoller MemInfoPoller.Config
    | DiskSpacePoller DiskSpacePoller.Config


