#maps.R

#' @export
#' @rdname maps


render_vacc_map <- function(sel_lang = "en"){
  if(sel_lang == c("de")){
    vacc_count$NAME_SHOW <- as.character(vacc_count$NAME_DE)
    Encoding(vacc_count$NAME_SHOW) <- "UTF-8"
  }else{
    vacc_count$NAME_SHOW <- as.character(vacc_count$NAME)
  }
  
  pal <- leaflet::colorFactor(palette = "Dark2", domain = (vacc_count$vaccine))
  
  leaflet::renderLeaflet({
    leaflet::leaflet(data = vacc_count, options = leafletOptions(maxZoom = 7, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
      leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
      leaflet::addMapPane(name = "maplabels", zIndex = 420) %>%
      leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
      #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
      leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal(vaccine), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
      #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
      leaflet::addLegend(position = "bottomright", pal = pal, values = ~vaccine, title = "", opacity = 0.7) %>%
      leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
  })  
  
}



render_bar_plot <- function(sel_lang = "en", relative = FALSE){
  #options(scipen = 100000000)
  
  plot_data <- dplyr::filter(vacc_count@data, Vaccinated > 0) 

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
  
  
  plot_data$NAME_SHOW <- factor(plot_data$NAME_SHOW, levels = unique(plot_data$NAME_SHOW)[order(plot_data$Vaccinated, decreasing = FALSE)])
  
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
