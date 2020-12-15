#app.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(shiny.semantic)
library(shiny.i18n)
library(leaflet)

setwd("C:/Dropbox/R/polivax")

i18n <- Translator$new(translation_json_path = "data/translation.json")
i18n$set_translation_language("en") # here you select the default translation to display


# Define UI for application that draws a histogram
ui <- semanticPage(
    title = "Polivax",
    shiny.i18n::usei18n(i18n),
    main_menu(),
    div(style = "float: right;",
            selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),
    leafletOutput("vacc_map"),
    titlePanel(i18n$t("Hello Shiny!"), windowTitle = NULL),
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        i18n$t("Number of bins:"), # you use i18n object as always
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
            plotOutput("distPlot"),
            p(i18n$t("This is the description of the plot."))
        )
    )
    #map_vaccine()
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$selected_language, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins,
             col = "darkgray", border = "white",
             main = i18n$t("Histogram of x"), ylab = i18n$t("Frequency"))
    })
    
    countries <- rgdal::readOGR("data/countries.geojson")
    vacc_count <- subset(countries, countries$ISO_A3 %in% c("GBR"))
    
    output$vacc_map <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>% 
            addPolygons(data = vacc_count, stroke = FALSE, smoothFactor = 0.3)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
