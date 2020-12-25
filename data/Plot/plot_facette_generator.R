library(tidyverse)
library(leaflet)
library(mapview)
library(polivax)

latest_vaccs <- read.csv("data/country_vaccine.csv")
country_historical <- read.csv("data/country_historical.csv", stringsAsFactors = TRUE, na.strings = c("", "NA"))
country_GDP <- read.csv("data/country_GDP.csv")
colnames(country_GDP) <- c("name", "ISO_A3", "year", "value")
country_GDP_tidy <- country_GDP %>% group_by(ISO_A3) %>% arrange(year) %>% summarise(year = last(year), GDP = last(value))
country_historical <- country_historical %>% mutate(ISO_A3 = iso_a3)
new_data <- merge(country_historical, country_GDP_tidy, by = "ISO_A3")

latest_vaccs <- tidyr::gather(latest_vaccs, key = "vaccine", value = "approved", -name, -iso_a2, -iso_a3, -Longitude, -Latitude, -Vaccinated) %>% 
  dplyr::filter(approved == 1) %>% 
  dplyr::mutate(ISO_A3 = iso_a3) 



latest_vaccs$vaccine <- factor(latest_vaccs$vaccine)
labels_vacc <- stringr::str_replace(levels(latest_vaccs$vaccine), "\\.", "\\-") %>% stringr::str_replace("_", " ")
latest_vaccs$vaccine <- factor(latest_vaccs$vaccine, levels = levels(latest_vaccs$vaccine), labels = labels_vacc)

latest_vaccs_first <- latest_vaccs %>% dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(vaccine = dplyr::first(vaccine), Vaccinated = max(Vaccinated))
latest_vaccs_second <- latest_vaccs %>% dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(vaccine = dplyr::nth(vaccine, 2), Vaccinated = max(Vaccinated)) %>% na.omit

color_frame <- merge(new_data, latest_vaccs_first, all.x = TRUE, by = "ISO_A3")


countries$NAME_SHOW <- as.character(countries$NAME)
countries$ISO_A3 <- countries$ADM0_A3

vacc_data <- countries
vacc_data@data <- dplyr::left_join(vacc_data@data, color_frame, by = "ISO_A3")
vacc_data$vaccine <- droplevels(vacc_data$vaccine)

#map for vaccinations
vacc_data_plot <- subset(vacc_data, !is.na(vacc_data$vaccine))
pal <- leaflet::colorFactor(palette = "Set2", domain = (vacc_data_plot$vaccine))

vacc_map <- leaflet::leaflet(data = vacc_data_plot, options = leafletOptions(maxZoom = 7, zoomControl = FALSE, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
    leaflet::addMapPane(name = "polygons_hatched", zIndex = 420) %>%
    leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
    #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
    leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal(vaccine), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
    #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
    leaflet::addLegend(position = "topright", pal = pal, values = ~vaccine, title = "", opacity = 0.7) %>%
    leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
  #leaflet::addPolygons(data = vacc_data_plot_hatch, stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = "none", fillOpacity = 0.8, group = "vaccine2", options = leafletOptions(pane = "polygons_hatched")) %>%

mapshot(vacc_map, file = "mapshot_vacc.png") 

#map for colonial empires
colonial_data <- subset(vacc_data, !is.na(vacc_data$colonial_empires))
colonial_data$colonial_empires <- factor(colonial_data$colonial_empires, labels = c("Colony", "Empire"))
pal_colonial <- leaflet::colorFactor(palette = c("#ffd92f", "#fc8d62"), domain = (colonial_data$colonial_empires))

colonial_map <- leaflet::leaflet(data = colonial_data, options = leafletOptions(maxZoom = 7, zoomControl = FALSE, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
  leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
  leaflet::addMapPane(name = "polygons_hatched", zIndex = 420) %>%
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
  #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
  leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal_colonial(colonial_empires), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
  #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
  leaflet::addLegend(position = "topright", pal = pal_colonial, values = ~colonial_empires, title = "", opacity = 0.7) %>%
  leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
#leaflet::addPolygons(data = colonial_data_hatch, stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = "none", fillOpacity = 0.8, group = "vaccine2", options = leafletOptions(pane = "polygons_hatched")) %>%

mapshot(colonial_map, file = "mapshot_colonial.png") 


#map for cold war
cold_data <- subset(vacc_data, !is.na(vacc_data$cold_war))
cold_data$cold_war <- factor(cold_data$cold_war, levels = c(1, 2), labels = c("West", "East"))
pal_cold <- leaflet::colorFactor(palette = c("#fc8d62", "#8da0cb"), domain = (cold_data$cold_war))

cold_map <- leaflet::leaflet(data = cold_data, options = leafletOptions(maxZoom = 7, zoomControl = FALSE, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
  leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
  leaflet::addMapPane(name = "polygons_hatched", zIndex = 420) %>%
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
  #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
  leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal_cold(cold_war), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
  #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
  leaflet::addLegend(position = "topright", pal = pal_cold, values = ~cold_war, title = "", opacity = 0.7) %>%
  leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
#leaflet::addPolygons(data = cold_data_hatch, stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = "none", fillOpacity = 0.8, group = "vaccine2", options = leafletOptions(pane = "polygons_hatched")) %>%

mapshot(cold_map, file = "mapshot_cold.png") 


#map for GDP
gdp_data <- subset(vacc_data, !is.na(vacc_data$INCOME_GRP))
gdp_data$INCOME_GRP <- factor(gdp_data$INCOME_GRP)

pal_gdp <- leaflet::colorFactor(palette = c("#FFFFFF", "#FEE2D7", "#FDC6B0", "#FCA989", "#FC8D62"), domain = (gdp_data$INCOME_GRP), reverse = TRUE)

gdp_map <- leaflet::leaflet(data = gdp_data, options = leafletOptions(maxZoom = 7, zoomControl = FALSE, worldCopyJump = TRUE, maxBounds = list(list(-90, -180),list(90, 180)))) %>% 
  leaflet::addMapPane(name = "polygons", zIndex = 410) %>% 
  leaflet::addMapPane(name = "polygons_hatched", zIndex = 420) %>%
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels, group = "map_base", options = providerTileOptions(noWrap = TRUE)) %>% 
  #addProviderTiles(providers$Stamen.TerrainLabels, options = leafletOptions(pane = "maplabels"), group = "map_labels") %>%
  leaflet::addPolygons(stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = ~pal_gdp(INCOME_GRP), label = ~NAME_SHOW, fillOpacity = 0.8, group = "vaccine", options = leafletOptions(pane = "polygons"), highlightOptions = highlightOptions(stroke = TRUE, bringToFront = TRUE, weight = 2, color = "white")) %>%
  #addLayersControl(baseGroups = "map_base", overlayGroups = c("map_labels", "vaccine")) %>%
  leaflet::addLegend(position = "topright", pal = pal_gdp, values = ~INCOME_GRP, title = "", opacity = 0.7) %>%
  leaflet::setView(lng = 0, lat = 20, zoom = 1.5)
#leaflet::addPolygons(data = gdp_data_hatch, stroke = FALSE, weight = 0, smoothFactor = 0.3, color = "none", fillColor = "none", fillOpacity = 0.8, group = "vaccine2", options = leafletOptions(pane = "polygons_hatched")) %>%

mapshot(gdp_map, file = "mapshot_gdp.png") 


  