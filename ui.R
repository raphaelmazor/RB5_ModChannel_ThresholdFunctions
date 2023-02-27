ui <- fluidPage(
  title = "Threshold Identification for Modified Channels and Other Stream Types",
  titlePanel("Threshold Identification for Modified Channels and Other Stream Types"),
  fluidRow(
    column(
      2,
      shinyWidgets::pickerInput(
        "Class_fullname", 
        label = "Class", 
        choices = class_choices,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE)
      ),
      shinyWidgets::pickerInput(
        "Stringency",
        label = "Stringency", 
        choices = stringency_choices,
        selected = stringency_choices |> dplyr::first()
      ),
      shinyWidgets::pickerInput(
        "Indicator",
        label = "Indicator", 
        choices = indicator_choices,
        selected = indicator_choices |> dplyr::first(),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE)
      ),
      DT::dataTableOutput("user_input_table")
    ),
    column(
      10,
      plotOutput("assessment_plot"),
      DT::dataTableOutput("threshold_table")
    )
  )
)