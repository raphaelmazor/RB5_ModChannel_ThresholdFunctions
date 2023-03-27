ui <- fluidPage(
  title = "Threshold Identification for Modified Channels and Other Stream Types",
  titlePanel("Threshold Identification for Modified Channels and Other Stream Types"),
  fluidRow(
    column(
      12,
      "This dashboard is intended to help managers identify thresholds for biointegrity and biostimulatory indicators for stream types where standard thresholds may not be desired. These stream types include: Intermittent streams (in northern and southern California), Central Valley Floor streams, and five classes of modified channels (i.e., hard-bottom engineered channels; soft-bottom engineered channels with 0, 1, or 2 hardened sides; and constructed channels)."
    ),
  ),
  fluidRow(
    column(
      4,
      br(),
      HTML('<strong style="font-size:16px;">Identify Thresholds</strong>'),
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
        selected = indicator_choices,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE)
      ),
      "Identify the thresholds you wish to evaluate. Under “Class”, select the appropriate stream types (traditionally-derived thresholds for wadeable streams will be included with results). Multiple options may be selected. Under “Stringency” select whether you want to evaluate high-, intermediate-, or low-stringency thresholds (only one stringency may be selected). Under “Indicator”, select which biointegrity or biostimulatory thresholds you want to evaluate.",
      br(),
      br(),
      HTML('<strong style="font-size:16px;">Enter Observed Values</strong>'),
      DT::dataTableOutput("user_input_table"),
      br(),
      div(style = "display:block; float:right",actionButton("clear", "Clear"), actionButton("submit", "Submit")),
      br(),
      br(),
      "Enter values observed at a site to evaluate with the thresholds identified above by double-clicking each cell in the Observed Value column. If data are missing, leave the observed value blank. Click the Submit button below the table to submit."
    ),
    column(
      8,
      align = "center",
      div(style = "display:block; float:left", downloadButton("download_graphic", "Download Graphic"), downloadButton("download_table", "Download Table")),
      plotOutput("assessment_plot", width = "6.5in", height = "7.5in"),
      DT::dataTableOutput("threshold_table")
    )
  )
)