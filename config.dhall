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

let InterfaceMetric =
    < GetTransmitRate :
        { interfaceName : Text
        , toWidget : ∀(current : Double) → ∀(previous : List Double) → Widget
        }
    | GetReceiveRate :
        { interfaceName : Text
        , toWidget : ∀(current : Double) → ∀(previous : List Double) → Widget
        }
    >

let StatMetric =
    < GetCpuUtilization :
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
    | InterfacePoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        , metrics : List InterfaceMetric
        }
    | StatPoller :
        { filepath : Text
        , interval : Natural
        , historyLength : Natural
        , metrics : List StatMetric
        }
    >


--let WidgetConfig =
--    < MeterWidgetConfig :
--      { title : Text
--      }
--    | NumberWidgetConfig :
--      { title : Text
--      , moreInfo : Text
--      }
--    >


let DiskSpacePollerConfig =
    { filepath : Text
    , interval : Natural
    , historyLength : Natural
    , widgetId : Text
    , widget : { title : Text}
    }


let diskSpaceTask : DiskSpacePollerConfig -> Task =
    λ(config : DiskSpacePollerConfig) ->
      Task.DiskSpacePoller
          { filepath = config.filepath
          , interval = config.interval
          , historyLength = config.historyLength
          , metrics =
              [ DiskSpaceMetric.GetDiskUsage
                  { toWidget =
                      λ(current : Double) → λ(previous : List Double) →
                          Widget.MeterWidget
                              { widgetId = config.widgetId
                              , meter =
                                  { title = config.widget.title
                                  , value = current
                                  }
                              }
                  }
              ]
          }


let defaultDiskSpaceConfig : { widgetId : Text } -> DiskSpacePollerConfig =
    λ(config : { widgetId : Text }) ->
        { filepath = "."
        , interval = 5
        , historyLength = 1
        , widgetId = config.widgetId
        , widget = { title = "Used space"}
        }
in
{ apiBaseUrl = "http://192.168.10.144:8080"
, tasks =
    [ diskSpaceTask (
        defaultDiskSpaceConfig { widgetId = "b" } //
            { filepath = "."}
      )
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
    , Task.InterfacePoller
        { filepath = "dev.txt"
        , interval = 5
        , historyLength = 1
        , metrics =
            [ InterfaceMetric.GetTransmitRate
                { interfaceName = "eno1"
                , toWidget =
                    λ(current : Double) → λ(previous : List Double) →
                        Widget.MeterWidget
                            { widgetId = "networkTransmit"
                            , meter =
                                { title = "Transmit rate"
                                , value = current
                                }
                            }
                }
            , InterfaceMetric.GetReceiveRate
                { interfaceName = "eno1"
                , toWidget =
                    λ(current : Double) → λ(previous : List Double) →
                        Widget.MeterWidget
                            { widgetId = "networkRecieve"
                            , meter =
                                { title = "Receive rate"
                                , value = current
                                }
                            }
				}
            ]
        }
    , Task.StatPoller
        { filepath = "stat.txt"
        , interval = 5
        , historyLength = 1
        , metrics =
            [ StatMetric.GetCpuUtilization
                { toWidget =
                    λ(current : Double) → λ(previous : List Double) →
                        Widget.MeterWidget
                            { widgetId = "cpuUtilization"
                            , meter =
                                { title = "Cpu load"
                                , value = current
                                }
                            }
                }
            ]
        }
    ]
}
