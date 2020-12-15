#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(shiny.semantic)
library(shiny.i18n)


setwd("C:/Dropbox/R/polivax")

translator <- Translator$new(translation_json_path = "data/translation.json")

translator$set_translation_language("en")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$page_content <- renderUI({
        fluidPage(main_menu(),
                  fluidRow(selectInput('selected_language',
                           i18n()$t("Change language"),
                           choices = translator$get_languages(),
                           selected = "en"))
        )
    })
})

