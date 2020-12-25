#maps.R

#' @name render_vacc_map
#' @title render_vacc_map
#' @usage render_vacc_map(sel_lang, latest_vaccs)
#' @description This function plots an interactive leaflet map with color polygons of vaccine approvals
#' @rdname render_vacc_map
#' @param sel_lang language for map labels. Currently supporting English "en" and German "de".
#' @param latest_vaccs A csv file giving the latest data on vaccinations with the following collumns required: iso_a2, iso_a3, Longitude, Latitude, Vaccinated and one collumn per vaccine manufacturer
#' @return shiny.render.function to return leaflet map
#' @import tidyr dplyr stringr leaflet ggplot2 plotly
#' @export


render_vacc_map <- function(sel_lang = "en", latest_vaccs = latest_vaccs){
  #prep data
  
  latest_vaccs <- tidyr::gather(latest_vaccs, key = "vaccine", value = "approved", -name, -iso_a2, -iso_a3, -Longitude, -Latitude, -Vaccinated) %>% 
    dplyr::filter(approved == 1) %>% 
    dplyr::mutate(ISO_A3 = iso_a3) %>% 
    dplyr::group_by(ISO_A3) 
  
  latest_vaccs$vaccine <- factor(latest_vaccs$vaccine)
  labels_vacc <- stringr::str_replace(levels(latest_vaccs$vaccine), "\\.", "\\-") %>% stringr::str_replace("_", " ")
  latest_vaccs$vaccine <- factor(latest_vaccs$vaccine, levels = levels(latest_vaccs$vaccine), labels = labels_vacc)
  
  latest_vaccs_first <- latest_vaccs %>% dplyr::group_by(ISO_A3) %>%
    dplyr::summarise(vaccine = dplyr::first(vaccine), Vaccinated = max(Vaccinated))
  latest_vaccs_second <- latest_vaccs %>% dplyr::group_by(ISO_A3) %>%
    dplyr::summarise(vaccine = dplyr::nth(vaccine, 2), Vaccinated = max(Vaccinated)) %>% na.omit
  
  countries$ISO_A3 <- countries$ADM0_A3
  if(sel_lang == c("de")){
    countries$NAME_SHOW <- as.character(countries$NAME_DE)
    Encoding(countries$NAME_SHOW) <- "UTF-8"
  }else{
    countries$NAME_SHOW <- as.character(countries$NAME)
  }
  
  vacc_data <- subset(countries, countries$ISO_A3 %in% latest_vaccs_first$ISO_A3)
  vacc_data@data <- dplyr::inner_join(vacc_data@data, latest_vaccs_first, by = "ISO_A3")
  vacc_data$vaccine <- droplevels(vacc_data$vaccine)
  pal <- leaflet::colorFactor(palette = "Dark2", domain = (vacc_data$vaccine))
  vacc_data_hatch <- subset(countries, countries$ISO_A3 %in% latest_vaccs_second$ISO_A3)
  vacc_data_hatch@data <- dplyr::inner_join(vacc_data_hatch@data, latest_vaccs_second, by = "ISO_A3")
  
  leaflet::renderLeaflet({
    leaflet::leaflet(data = vacc_data, options = leafletOptions(maxZoom = 7, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
      leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
      leaflet::addMapPane(name = "polygons_hatched", zIndex = 420) %>%
      leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
      #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
      leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal(vaccine), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
      #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~vaccine, title = "", opacity = 0.7) %>%
      leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
      #leaflet::addPolygons(data = vacc_data_hatch, stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = "none", fillOpacity = 0.8, group = "vaccine2", options = leafletOptions(pane = "polygons_hatched")) %>%
  })  
  
}

#' @name render_bar_plot
#' @usage render_bar_plot(sel_lang, relative, vacc_data)
#' @description This function plots an interactive bar chart using plotly to show number or proportion of people vaccinated within a country
#' @rdname render_bar_plot
#' @title render_bar_plot
#' @param sel_lang language for map labels. Currently supporting English "en" and German "de".
#' @param relative Boolean value to control whether the function outputs a barchart with relative or absolute numbers
#' @param vacc_data dataset returned from \link[polivax]{prep_data} function
#' @return shiny.render.function to return plotly barchart 
#' @export


render_bar_plot <- function(sel_lang = "en", relative = FALSE, vacc_data = vacc_count){
  #options(scipen = 100000000)
  
  plot_data <- dplyr::filter(vacc_data@data, Vaccinated > 0) 

  if(sel_lang == c("de")){
    plot_data$NAME_SHOW <- as.character(plot_data$NAME_DE)
    Encoding(plot_data$NAME_SHOW) <- "UTF-8"
  }else{
    plot_data$NAME_SHOW <- as.character(plot_data$NAME)
  }
  #plot_data$NAME_SHOW <- factor(plot_data$NAME_SHOW)
  
  plot_data$vacc_rel <- (plot_data$Vaccinated/plot_data$POP_EST)*100
  
  if(relative == TRUE){
    plot_data$y_plot <- plot_data$vacc_rel
  }else{
    plot_data$y_plot <- plot_data$Vaccinated
  }
  
  
  plot_data$NAME_SHOW <- factor(plot_data$NAME_SHOW, levels = unique(plot_data$NAME_SHOW)[order(plot_data$vacc_rel, decreasing = FALSE)])
  
  colorScale <- leaflet::colorFactor(domain = plot_data$NAME_SHOW, palette = "YlGn")
  
  renderPlotly({
    
    plotly::plot_ly(data = plot_data, 
            x= ~y_plot, 
            y= ~NAME_SHOW, 
            type = "bar", 
            marker = list(color = c(colorScale(plot_data$NAME_SHOW))), 
            orientation = 'h', 
            height = nrow(plot_data)*75) %>%
      plotly::layout(autosize = T,  xaxis = list(title = ""), yaxis = list(title = ""))
    
    
    # ggplot(data = plot_data, aes(x = NAME_SHOW, y = y_plot, fill = NAME_SHOW))+
    #   geom_bar(stat = "identity", width = 0.6) +
    #   coord_flip() +
    #   scale_fill_brewer(palette = "Set2") +
    #   theme_minimal()+
    #   theme(axis.title.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.text.y = element_text(angle = 45, size = 15),
    #         axis.text.x = element_text(size = 15),
    #         panel.grid.minor = element_blank(),
    #         legend.position = "none",
    #         aspect.ratio = 1/3)
  })
  
}
