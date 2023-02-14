ui <- fluidPage(
  fluidRow(
    column(
      2,
      selectInput(
        "Class_fullname", 
        label = "Class", 
        choices = class_choices,
        multiple = TRUE
      ),
      selectInput(
        "Stringency",
        label = "Stringency", 
        choices = stringency_choices,
        selected = stringency_choices |> dplyr::first(),
        multiple = TRUE
      ),
      selectInput(
        "Indicator",
        label = "Indicator", 
        choices = indicator_choices,
        selected = indicator_choices |> dplyr::first(),
        multiple = TRUE
      )
    ),
    column(
      10,
      dataTableOutput("threshold_table")
    )
  )
)