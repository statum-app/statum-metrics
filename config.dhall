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
in
{ widgetFunctions =
    { diskUsageWidget =
        λ(current : Double) → λ(previous : List Double) →
            Widget.MeterWidget
                { widgetId = "freeSpace"
                , meter =
                    { title = "Free space"
                    , value = current
                    }
                }
    }
}
