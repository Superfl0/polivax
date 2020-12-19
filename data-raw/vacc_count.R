## code to prepare `vacc_count` dataset goes here

countries <- rgdal::readOGR("data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp",layer = "ne_50m_admin_0_countries", GDAL1_integer64_policy = TRUE)
countries_vaccine <- read.csv("data/country_vaccine.csv")


countries_vaccine <- tidyr::gather(countries_vaccine, key = "vaccine", value = "approved", -name, -iso_a2, -iso_a3, -Longitude, -Latitude, -Vaccinated) %>% 
  dplyr::filter(approved == 1) %>% 
  dplyr::mutate(ISO_A3 = iso_a3) %>% 
  dplyr::group_by(ISO_A3) %>% 
  dplyr::summarise(vaccine = dplyr::first(vaccine), Vaccinated = max(Vaccinated))

countries_vaccine$vaccine <- factor(countries_vaccine$vaccine)
labels_vacc <- stringr::str_replace(levels(countries_vaccine$vaccine), "\\.", "\\-") %>% stringr::str_replace("_", " ")
countries_vaccine$vaccine <- factor(countries_vaccine$vaccine, levels = levels(countries_vaccine$vaccine), labels = labels_vacc)

vacc_count <- subset(countries, countries$ISO_A3 %in% countries_vaccine$ISO_A3)

vacc_count@data <- dplyr::inner_join(vacc_count@data, countries_vaccine, by = "ISO_A3")

usethis::use_data(vacc_count, overwrite = TRUE)
