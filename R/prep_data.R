#' Data Preparation Function
#' 
#' A function to merge the latest vaccination data with country polygons
#' 
#' @name prep_data
#' @param latest_vaccs A csv file giving the latest data on vaccinations with the following collumns required: iso_a2, iso_a3, Longitude, Latitude, Vaccinated and one collumn per vaccine manufacturer
#' @keywords datawrangling
#' @export
#' @import tidyr dplyr stringr

prep_data <- function(latest_vaccs){
  
  latest_vaccs <- tidyr::gather(latest_vaccs, key = "vaccine", value = "approved", -name, -iso_a2, -iso_a3, -Longitude, -Latitude, -Vaccinated) %>% 
    dplyr::filter(approved == 1) %>% 
    dplyr::mutate(ISO_A3 = iso_a3) %>% 
    dplyr::group_by(ISO_A3) %>% 
    dplyr::summarise(vaccine = dplyr::first(vaccine), Vaccinated = max(Vaccinated))
  
  latest_vaccs$vaccine <- factor(latest_vaccs$vaccine)
  labels_vacc <- stringr::str_replace(levels(latest_vaccs$vaccine), "\\.", "\\-") %>% stringr::str_replace("_", " ")
  latest_vaccs$vaccine <- factor(latest_vaccs$vaccine, levels = levels(latest_vaccs$vaccine), labels = labels_vacc)
  
  countries$ISO_A3 <- countries$SU_A3
  vacc_data <- subset(countries, countries$ISO_A3 %in% latest_vaccs$ISO_A3)
  
  vacc_data@data <- dplyr::inner_join(vacc_data@data, latest_vaccs, by = "ISO_A3")
  
  return(vacc_data)
}


