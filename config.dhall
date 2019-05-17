let Meter =
    { title : Text
    , value : Double
    }

let Number =
    { title : Text
    , current : Double
    , previous : Double
    , moreInfo : Text
    }

let Widget =
    < MeterWidget : { widgetId : Text, meter : Meter }
    | NumberWidget : { widgetId : Text, number : Number }
    >

--let DiskSpacePollerConfig =
--    { filepath : Text
--    , interval : Natural
--    , historyLength : Natural
--    }
--
--let MemInfoPollerConfig =
--    { filepath : Text
--    , interval : Natural
--    , historyLength : Natural
--    }

let Task =
    < DiskSpacePoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        }
    | MemInfoPoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        }
    >
in
{ tasks =
    [ Task.DiskSpacePoller
        { filepath = "."
        , interval = 5
        , historyLength = 1
        }
    , Task.MemInfoPoller
        { filepath = "meminfo.txt"
        , interval = 5
        , historyLength = 1
        }
    ]
}


    --{ diskUsageWidget =
    --    λ(current : Double) → λ(previous : List Double) →
    --        Widget.MeterWidget
    --            { widgetId = "freeSpace"
    --            , meter =
    --                { title = "Free space"
    --                , value = current
    --                }
    --            }
    --}
