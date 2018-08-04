
#' Hue API: sensors endpoints
#'
#' @param id id of a specific sensor
#' @param name name to assign to the sensor
#' @param ... named parameters describing sensor state (e.g. \code{on = TRUE})
#' @param return_id when creating a new resource, the default is to return
#'   \code{TRUE} (invisibly) upon success; setting \code{return_id = TRUE} will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return \code{TRUE} (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/sensors-api}
#'
#' @name sensors

#' @rdname sensors
#' @export
create_sensor <- function(..., return_id = FALSE) {
    path <- bridge_url('sensors')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname sensors
#' @export
search_for_new_sensors <- function() {
    path <- bridge_url('sensors')
    y <- httr::POST(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname sensors
#' @export
get_new_sensors <- function() {
    path <- bridge_url('sensors', 'new')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname sensors
#' @export
rename_sensor <- function(id, name) {
    path <- bridge_url('sensors', id)
    y <- httr::PUT(path, body = list(name = name), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname sensors
#' @export
get_sensors <- function() {
    path <- bridge_url('sensors')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname sensors
#' @export
get_sensor <- function(id) {
    path <- bridge_url('sensors', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname sensors
#' @export
set_sensor_config <- function(id, ...) {
    path <- bridge_url('sensors', id, 'config')
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname sensors
#' @export
set_sensor_state <- function(id, ...) {
    path <- bridge_url('sensors', id, 'state')
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname sensors
#' @export
delete_sensor <- function(id) {
    path <- bridge_url('sensors', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' Configure Built-In Daylight Sensor
#'
#' Supported sensors for the Hue Bridge include a virtual daylight sensor that
#' calculates sunrise and sunset times based on your location. This function
#' helps configure the built-in daylight sensor (\code{id = 1}).
#'
#' @param lat latitude (in decimal degrees). Positive north; negative south.
#' @param lon longitude (in decimal degrees). Positive east; negative west.
#' @param sunriseoffset "daylight" begins \code{sunriseoffset} minutes after
#'   sunrise
#' @param sunsetoffset "daylight" ends \code{sunsetoffset} minutes after sunset
#' @param id ID of the daylight sensor
#'
#' @return Returns \code{TRUE} (invisibly) uppon success.
#'
#' @seealso \url{https://www.developers.meethue.com/documentation/supported-sensors}
#'
#' @export
configure_daylight_sensor <- function(lat, lon, sunriseoffset = 30, sunsetoffset = -30, id = 1) {
    set_sensor_config(
        id = id,
        lat = ifelse(
            lat < 0,
            sprintf('%03.4fS', abs(lat)),
            sprintf('%03.4fN', abs(lat))
        ),
        long = ifelse(
            lon < 0,
            sprintf('%03.4fW', abs(lon)),
            sprintf('%03.4fE', abs(lon))
        ),
        sunriseoffset = sunriseoffset,
        sunsetoffset = sunsetoffset
    )
}
