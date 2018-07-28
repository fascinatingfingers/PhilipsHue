
#' Hue API: sensors endpoints
#'
#' @param id id of a specific sensor, an integer
#' @param name name to give to a specific sensor
#' @param ... named parameters describing sensor state (e.g. \code{on = TRUE})
#'
#' @return Requests with side effects invisibly return \code{TRUE} upon success.
#'   GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/sensors-api}
#'
#' @name sensors

#' @rdname sensors
#' @export
create_sensor <- function(...) {
    path <- bridge_url('sensors')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
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
