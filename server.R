library(ggplot2)

server <- function(input, output, session) {
  
  
  
  threshold_data <- reactive({
    threshold_static |>
      dplyr::filter(
        Class_fullname %in% c("Wadeable streams", input$Class_fullname),
        Stringency %in% input$Stringency,
        Indicator %in% input$Indicator
      ) |>
      dplyr::mutate(
        Class = as.factor(Class),
        Approach = as.factor(Approach),
        Response_model_form = as.factor(Response_model_form),
        Response_model_index = as.factor(Response_model_index),
        Approach4 = dplyr::case_when(
          Approach != "Response" ~ paste0(Class," - ",Approach),
          Approach == "Response" ~ paste0(Class," - ",Approach," (",Response_model_form,", ", Response_model_index,")"),
                               T ~ "OTHER"
        ),
        Approach4 = factor(Approach4, 
                           levels = Approach4 %>%
                             unique() %>%
                             rev())
      ) |>
      dplyr::arrange(
        Class,
        Approach,
        Response_model_form,
        Response_model_index
      )
  }) |>
    bindEvent(input$Class_fullname, input$Stringency, input$Indicator)
    
    
  my_thresh_df <- reactive({
    threshold_data() |>
      dplyr::inner_join(v$data) |>
      dplyr::mutate(Threshold_pass = dplyr::case_when(
        is.na(Observed_value)~"No data",
        is.na(Threshold_value)~"No threshold identified",
        Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & is.na(Flag) ~ "Fails",
        Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & is.na(Flag) ~ "Passes",
        Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & !is.na(Flag) ~ "Fails but flagged",
        Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & !is.na(Flag) ~ "Passes flagged",
        
        Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & is.na(Flag) ~ "Fails",
        Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & is.na(Flag) ~ "Passes",
        Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & !is.na(Flag) ~ "Fails but flagged",
        Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & !is.na(Flag) ~ "Passes flagged",
        
        T~"Other"),
        Threshold_pass = as.factor(Threshold_pass),
        obs_label = paste0(Indicator,"\n(",Observed_value,")")
      )
  })
  

  output$assessment_plot <- renderPlot({
    assessment_plot <- ggplot(data=my_thresh_df(), aes(x=obs_label, y=Approach4)) +
      geom_tile(aes(fill=Threshold_pass), color="white") +
      geom_text(aes(label=Threshold_value))+
      facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
      scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#cab2d6","#ff7f00","#fdbf6f"), name="Threshold", drop=F)+
      ylab("")+
      xlab("Indicator\n(Observed value)")+
      theme_bw()+
      theme(legend.position = "bottom",
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(color="gray25"))
    
    assessment_plot_cow <- cowplot::plot_grid(assessment_plot +
                                              theme(legend.position = "none"),
                                            cowplot::get_legend(assessment_plot),
                                            nrow=2, rel_heights=c(1,.2))
    assessment_plot_cow
  }) 
    
  
  v <- reactiveValues(data = {
    data.frame(Indicator = indicator_choices,
               Observed_value = c(NA_real_, 9.2, 0.97, 1.02, 0.39, 13.26, 0.79, 1.03))
    
  })
  
  output$user_input_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE, options = list(dom = 't'), selection = 'none')
  })
  
  
  observe({
    info <- input$user_input_table_cell_edit
    i <- as.numeric(info$row)
    j <- as.numeric(info$col)
    k <- as.numeric(info$value)
    
    v$data[i, j] <- k
  }) |>
    bindEvent(input$user_input_table_cell_edit)
  
  output$threshold_table <- DT::renderDataTable({
    threshold_data() |>
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
  })
  
}