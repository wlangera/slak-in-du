get_visit_stats <- function(data, utm_kust) {
  # Neem intersectie met UTM-grid
  intersection <- utm_kust %>% 
    st_intersection(data) %>%
    
    # Selecteer per buffer het grootste deel
    mutate(oppervlakte = st_area(geometry)) %>%
    group_by(id) %>%
    filter(oppervlakte == max(oppervlakte))
  
  out_stats <- intersection %>%
    st_drop_geometry() %>%
    group_by(TAG) %>% 
    summarise(n_bezoeken = n_distinct(datum))
  
  out <- utm_kust %>%
    st_drop_geometry() %>%
    left_join(out_stats, by = "TAG") %>%
    select(TAG, n_bezoeken) %>%
    mutate(n_bezoeken = as.integer(ifelse(is.na(n_bezoeken), 0, n_bezoeken)))
  
  return(out)
}

stats_utm_grid <- function(data, soortnaam = NULL, resolutie, variabele = NULL) {
  # Error handling
  if (!is.numeric(resolutie)) {
    stop("Resolutie een numerieke waarde zijn!")
  }
  if (!(resolutie %in% c(1, 2, 5, 10))) {
    stop("Resolutie moet op 1, 2, 5 of 10 km schaal zijn!")
  }
  
  path <- paste0("C:/Users/WARD/Documents/slakken/Slak-in-Du/analyses/", 
                 "slak-in-du/data/processed")
  
  # Neem UTM-grid studiegebied afhankelijk van resolutie
  if (resolutie == 1) {
    utm_1km_kust <- st_read(paste(path, "utm_1km_kust.shp", sep = "/"),
                            quiet = TRUE)
    utm_kust <- utm_1km_kust
  } else if (resolutie == 2) {
    utm_2km_kust <- st_read(paste(path, "utm_2km_kust.shp", sep = "/"),
                            quiet = TRUE)
    utm_kust <- utm_2km_kust
  } else if (resolutie == 5) {
    utm_5km_kust <- st_read(paste(path, "utm_5km_kust.shp", sep = "/"),
                            quiet = TRUE)
    utm_kust <- utm_5km_kust
  } else {
    utm_10km_kust <- st_read(paste(path, "utm_10km_kust.shp", sep = "/"),
                             quiet = TRUE)
    utm_kust <- utm_10km_kust
  }
  suppressWarnings({
    
    if (is.null(variabele)) {
      # Selecteer dataframe voor soort
      df <- data %>%
        filter(ned_soortnaam == soortnaam)
    } else {
      df <- data
    }
    
    # Neem intersectie met UTM-grid
    intersection <- utm_kust %>% 
      st_intersection(df) %>%
      
      # Selecteer per buffer het grootste deel
      mutate(oppervlakte = st_area(geometry)) %>%
      group_by(id) %>%
      filter(oppervlakte == max(oppervlakte))
    
    if (is.null(variabele)) {
      # Bereken welke UTM-hokken zijn bezocht
      bezoeken <- get_visit_stats(data, utm_kust)
      
      # Lijst met UTM-hokken waar de soort voorkomt
      utm_list <- unique(intersection$TAG)
      
      # Add variable to resolution grid if species is present
      out <- utm_kust %>% 
        left_join(bezoeken, by = "TAG") %>%
        mutate(occurrence = ifelse(TAG %in% utm_list, "aanwezig", 
                                   ifelse(n_bezoeken == 0, NA, "afwezig")))
    } else {
      out_stats <- intersection %>%
        st_drop_geometry() %>%
        group_by(TAG) %>% 
        summarise(n = n_distinct(.data[[variabele]]),
                  n_bezoeken = n_distinct(datum))
      
      # Join counts aan UTM grid
      out <- left_join(utm_kust, out_stats, by = "TAG") %>%
        select(TAG, n, n_bezoeken) %>%
        mutate(n_bezoeken = as.integer(ifelse(is.na(n_bezoeken), 0, 
                                              n_bezoeken)))
    }
    
    # Return
    return(out)
  })
}