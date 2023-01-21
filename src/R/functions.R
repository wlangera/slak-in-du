stats_utm_grid <- function(soortnaam = NULL, resolutie, variabele = NULL) {
  # Error handling
  if (!is.numeric(resolutie)) {
    stop("Resolutie een numerieke waarde zijn!")
  }
  if (!(resolutie %in% c(1, 2, 5, 10))) {
    stop("Resolutie moet op 1, 2, 5 of 10 km schaal zijn!")
  }
  
  # Neem UTM-grid studiegebied afhankelijk van resolutie
  if (resolutie == 1) {
    utm_kust <- utm_1km_kust
  } else if (resolutie == 2) {
    utm_kust <- utm_2km_kust
  } else if (resolutie == 5) {
    utm_kust <- utm_5km_kust
  } else {
    utm_kust <- utm_10km_kust
  }
  suppressWarnings({
    
    if (is.null(variabele)) {
      # Selecteer dataframe voor soort
      data <- slakken_data_sf_buffer %>%
        filter(ned_soortnaam == soortnaam)
    } else {
      data <- slakken_data_sf_buffer
    }
    
    # Neem intersectie met UTM-grid
    intersection <- utm_kust %>% 
      st_intersection(data) %>%
      
      # Selecteer per buffer het grootste deel
      mutate(oppervlakte = st_area(geometry)) %>%
      group_by(id) %>%
      filter(oppervlakte == max(oppervlakte))
    
    if (is.null(variabele)) {
      # Lijst met UTM-hokken waar de soort voorkomt
      utm_list <- unique(intersection$TAG)
      
      # Add variable to resolution grid if species is present
      out <- utm_kust %>% 
        mutate(occurrence = ifelse(TAG %in% utm_list, "present", "absent"))
    } else {
      out_stats <- intersection %>%
        st_drop_geometry() %>%
        group_by(TAG) %>% 
        summarise(n = n_distinct(.data[[variabele]]))
      
      # Join aan UTM grid
      out <- left_join(utm_kust, out_stats, by = "TAG") %>%
        select(TAG, n) %>%
        mutate(n = as.integer(ifelse(is.na(n), 0, n)))
    }
    
    # Return
    return(out)
  })
}