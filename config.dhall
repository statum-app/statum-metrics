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
    < MeterWidget :
        { widgetId : Text
        , meter : Meter
        }
    | NumberWidget :
        { widgetId : Text
        , number : Number
        }
    >

let DiskSpaceMetric =
    < GetDiskUsage :
        { toWidget : ∀(current : Double) → ∀(previous : List Double) → Widget
        }
    >

let MemInfoMetric =
    < GetMemUsage :
        { toWidget : ∀(current : Double) → ∀(previous : List Double) → Widget
        }
    >

let Task =
    < DiskSpacePoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        , metrics : List DiskSpaceMetric
        }
    | MemInfoPoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        , metrics : List MemInfoMetric
        }
    >
in
{ tasks =
    [ Task.DiskSpacePoller
        { filepath = "."
        , interval = 5
        , historyLength = 1
        , metrics =
            [ DiskSpaceMetric.GetDiskUsage
                { toWidget =
                    λ(current : Double) → λ(previous : List Double) →
                        Widget.MeterWidget
                            { widgetId = "freeSpace"
                            , meter =
                                { title = "Free space"
                                , value = current
                                }
                            }
                }
            ]
        }
    , Task.MemInfoPoller
        { filepath = "meminfo.txt"
        , interval = 5
        , historyLength = 1
        , metrics =
            [ MemInfoMetric.GetMemUsage
                { toWidget =
                    λ(current : Double) → λ(previous : List Double) →
                        Widget.MeterWidget
                            { widgetId = "memUsage"
                            , meter =
                                { title = "Used memory"
                                , value = current
                                }
                            }
                }
            ]
        }
    ]
}
