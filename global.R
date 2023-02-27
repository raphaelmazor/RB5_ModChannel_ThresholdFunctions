threshold_static <- readr::read_csv("Data/mod_channel_thresholds.csv")


class_choices <- threshold_static |> 
  dplyr::distinct(Class_fullname) |> 
  dplyr::filter(Class_fullname != "Wadeable streams") |> 
  dplyr::pull()

stringency_choices <- threshold_static |> 
  dplyr::distinct(Stringency) |> 
  dplyr::arrange(desc(Stringency)) |>
  dplyr::pull()

indicator_choices <- threshold_static |> 
  dplyr::distinct(Indicator) |> 
  dplyr::arrange(Indicator) |>
  dplyr::pull()