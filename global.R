threshold_static <- readr::read_csv("Data/mod_channel_thresholds.csv") |>
  dplyr::select(
    Class_fullname, 
    Stringency,
    Approach,
    Response_model_detail,
    Indicator_Type,
    Indicator,
    Threshold_value,
    Flag
  )

class_choices <- threshold_static |> 
  dplyr::distinct(Class_fullname) |> 
  dplyr::filter(Class_fullname != "Wadeable streams") |> 
  dplyr::pull()

stringency_choices <- threshold_static |> 
  dplyr::distinct(Stringency) |> 
  dplyr::pull()

indicator_choices <- threshold_static |> 
  dplyr::distinct(Indicator) |> 
  dplyr::pull()