library(shiny)
library(DT)
library(dplyr)
library(readr)
source('ui.R', local = TRUE)
source('server.R')
source('global.R')

shinyApp(ui = ui, server = server)