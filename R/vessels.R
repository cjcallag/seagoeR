#' @title Get Location Vessels
#' 
#' @description returns Vessels within a 100 mile radius of a specified latitude and longitude
#'
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#'
#' @param api_key, API access token
#' @param latitude, latitude of the center point for the region of interest
#' @param longitude, longitude of the center point for the region of interest
#' @param age, how old the vessels that are returned can be in hours(must be between 1 and 720 hours)
#' @param radious, number of miles to search for vessels (must be 100 miles or under )
#' 
#' @importFrom httr add_headers content GET
#'
get_location_vessels <- function(api_key, latitude, longitude, age = NULL, radious = NULL) {
  #TODO add age and radious conditionally
  if (!is.character(api_key)) {
    stop("api_key not a string.",
         call. = FALSE)
  }
  if (!is.numeric(latitude) | !is.numeric(longitude)) {
    stop("longitude and latitude must be numeric.",
         call. = FALSE)
  }
  
  url <- paste0("https://api.seavision.volpe.dot.gov/v1/vessels?latitude=",
                latitude,
                "&longitude=",
                longitude)
  
  h        <- c(api_key)
  names(h) <- "x-api-key" 
  
  init <- GET(url,
              add_headers(.headers = h
                          )
              )
  
  out <- content(init)
  out
}


#' @title Get Vessel History
#' 
#' @description returns history trail for a specific vessel
#'
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#'
#' @param api_key, API access token
#' @param mmsi, mmsi tied to the vessel to get history trail for
#' @param age, how old the vessels that are returned can be in hours(must be between 1 and 720 hours)
#' 
#' @importFrom httr add_headers content GET
#' 
get_vessel_history <- function(api_key, mmsi, age = NULL) {
  #TODO add age conditionally
  if (!is.character(api_key)) {
    stop("api_key not a string.",
         call. = FALSE)
  }
  if (!is.numeric(mmsi)) {
    stop("mmsi must be numeric.",
         call. = FALSE)
  }
  
  url <- paste0("https://api.seavision.volpe.dot.gov/v1/vessels/",
                mmsi,
                "/history")
  
  h        <- c(api_key)
  names(h) <- "x-api-key" 
  
  init <- GET(url,
              add_headers(
                .headers = h
                          )
              )
  
  out <- content(init)
  out
}


#' @title Get Latest Vessel Transmission
#' 
#' @description returns latest vessel transmission from history trail
#'
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#'
#' @param api_key, API access token
#' @param mmsi, mmsi tied to the vessel to get history trail for
#'
get_latest_location <- function(api_key, mmsi) {
  if (!is.character(api_key)) {
    stop("api_key not a string.",
         call. = FALSE)
  }
  if (!is.numeric(mmsi)) {
    stop("mmsi must be numeric.",
         call. = FALSE)
  }
  
  history <- get_vessel_history(api_key = api_key, mmsi = mmsi)[[1]]
  history
}


#' @title Get Vessel Attributes
#' 
#' @description returns ship attributes (e.g., mmsi, imoNumber, name, callSign, cargo, vesselType, COG, heading, navStatus, SOG, latitude, longitude, timeOfFix, lenght, beam) based on the vessels last location
#'
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#'
#' @param api_key, API access token
#' @param mmsi, mmsi tied to the vessel to get history trail for
#'
get_vessel_attributes <- function(api_key, mmsi) {
  if (!is.character(api_key)) {
    stop("api_key not a string.",
         call. = FALSE)
  }
  if (!is.numeric(mmsi)) {
    stop("mmsi must be numeric.",
         call. = FALSE)
  }
  latest <- get_latest_location(api_key, mmsi)
  attrs <- get_location_vessels(api_key   = api_key,
                                latitude  = latest[["latitude"]],
                                longitude = latest[["longitude"]])
  
  df <- do.call(rbind, lapply(attrs, as.data.frame))
  df[df[["mmsi"]] == mmsi, ]
}
