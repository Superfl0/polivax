#app.R

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shiny.semantic)
library(shiny.i18n)
library(leaflet)
library(polivax)
library(plotly)


i18n <- Translator$new(translation_json_path = "www/translation.json")
i18n$set_translation_language("en") # here you select the default translation to display

css <- "
.div{
  padding: 0px 0px 0px 0px;
}
.icon{
  padding: 0px 0px 0px 0px;
}
.leaflet-container {
  background: white;
}
"

# Define UI for application that draws a histogram
ui <- semanticPage(margin = "0px",
    title = "Polivax",
    tags$head(tags$style(HTML(css))),
    shiny.i18n::usei18n(i18n),
    ui_header(i18n),
    leafletOutput("vacc_map"),
    div(class = "ui container", style = "padding: 20px 20px 20px 20px; background: white; max-width = 1000px;",
    
        
        box_ui(div(
                h1(class = "ui header", style = "text-align:center;", i18n$t("Total number of COVID-19 vaccination doses administered")),
                div(plotlyOutput("plot_vaccinated", width = "100%", height = "100%"))
                #div(style = "text-align:center;", i18n$t("Total number of COVID-19 vaccination doses administered"))
               )),
        box_ui(div(
            h1(class = "ui header", style = "text-align:center;", i18n$t("Number of COVID-19 vaccination doses administered per 100 inhabitants")),
            div(plotlyOutput("plot_vaccinated_relative", width = "100%", height = "100%"))
            #div(style = "text-align:center;", i18n$t("Total number of COVID-19 vaccination doses administered"))
            )),
        
        
        htmlOutput("methods_tag"),
        
        h1(class = "ui header", style = "text-align:center;", i18n$t("Methods")),
        
        box_ui(includeMarkdown("www/methods.Rmd"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    latest_vaccs <- read.csv("www/country_vaccine.csv")
    
    vacc_data <- prep_data(latest_vaccs = latest_vaccs)
    
    observeEvent(input$selected_language, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
        output$vacc_map <- render_vacc_map(sel_lang = input$selected_language, latest_vaccs = latest_vaccs)
        output$plot_vaccinated <- render_bar_plot(sel_lang = input$selected_language, relative = FALSE, vacc_data = vacc_data)
        output$plot_vaccinated_relative <- render_bar_plot(sel_lang = input$selected_language, relative = TRUE, vacc_data = vacc_data)
    })
    
    
    output$methods_tag <- renderText({
        "<div id='methods'>&nbsp;</div>"
    })
    
    #output$methods <- markdown("data/methods.rmd")
    
    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins,
             col = "darkgray", border = "white",
             main = i18n$t("Histogram of x"), ylab = i18n$t("Frequency"))
    })
    
    
    
 
}


# Run the application 
shinyApp(ui = ui, server = server)
