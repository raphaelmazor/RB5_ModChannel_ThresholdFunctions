server <- function(input, output, session) {
  #print(threshold_static)

  
  threshold_data <- reactive({
     threshold_static |>
      dplyr::filter(
        Class_fullname %in% c("Wadeable streams", input$Class_fullname),
        Stringency %in% input$Stringency,
        Indicator %in% input$Indicator
      )
  }) |>
    bindEvent(input$Class_fullname, input$Stringency, input$Indicator)
    
    
    
  
  output$threshold_table <- DT::renderDataTable({
    threshold_data()
  })
  
}