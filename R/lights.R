
#' Hue API: lights endpoints
#'
#' @param id id of a specific light
#' @param name name to assign to the light
#' @param ... named parameters describing light state (e.g. `on = TRUE`)
#'
#' @return Requests with side effects return `TRUE` (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso <https://developers.meethue.com/documentation/lights-api>
#'
#' @name lights

#' @rdname lights
#' @export
search_for_new_lights <- function() {
    path <- bridge_url('lights')
    y <- httr::POST(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname lights
#' @export
get_new_lights <- function() {
    path <- bridge_url('lights', 'new')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname lights
#' @export
rename_light <- function(id, name) {
    path <- bridge_url('lights', id)
    y <- httr::PUT(path, body = list(name = name), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname lights
#' @export
get_lights <- function() {
    path <- bridge_url('lights')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname lights
#' @export
get_light <- function(id) {
    path <- bridge_url('lights', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname lights
#' @export
set_light_state <- function(id, ...) {
    path <- bridge_url('lights', id, 'state')
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname lights
#' @export
delete_light <- function(id) {
    path <- bridge_url('lights', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
