makeHTML <- function(htmlText){
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}

buildGoogleMapsURLFromLatLong <- function(myLati, myLongi) {
  result <- paste0('<br>',
                   '<a rel = "noreferrer" href="https:www.google.co.uk/maps/@', 
                   gsub(" ", "", format(round(myLati, 8), nsmall = 8)),
                   ",", 
                   gsub(" ", "", format(round(myLongi, 8), nsmall = 8)), 
                   ',19z" target="_blank">',
                   'Show in Googlemaps</a>')
}


buildGoogleMapsURLFromStGeom <- function(thisGeom) {
  xy <- st_coordinates(thisGeom)
  theRes <- buildGoogleMapsURLFromLatLong(xy[,2], xy[,1])
}

expand_geom <- function(data_f, geom_col) {
  result <- data_f %>%
    mutate(poly_type = stringr::word({{geom_col}}, 1)) %>%
    mutate(xy = substr({{geom_col}}, 8, stringr::str_length({{geom_col}})-1)) %>%
    tidyr::separate(xy, c("geom_e", "geom_n"), " ") %>%
    mutate_at(vars(matches("geom_")), as.numeric)
  
  return(result)
}
