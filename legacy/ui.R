#UI.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(shiny.semantic)
library(shiny.i18n)

# Define UI for application that draws a histogram
ui <- semanticPage(
    title = "Polivax",
    uiOutput('page_content')
    #map_vaccine()
)
