
#' Hue API: light endpoints
#'
#' @param id id of a specific light, an integer
#' @param name name to give to a specific light
#' @param ... named parameters describing light state
#'
#' @return Returns the response content, conveniently parsed into an R-friendly
#'   data format.
#'
#' @name lights

#' @rdname lights
#' @export
search_for_new_lights <- function() {
    path <- bridge_url('lights')

    y <- httr::POST(path)

    return(httr::status_code(y) == 200)
}

#' @rdname lights
#' @export
get_new_lights <- function() {
    path <- bridge_url('lights', 'new')

    y <- httr::GET(path)
    y <- httr::content(y, 'text', encoding = 'UTF-8')
    y <- jsonlite::fromJSON(y, simplifyVector = FALSE)

    return(y)
}

#' @rdname lights
#' @export
rename_light <- function(id, name) {
    path <- bridge_url('lights', id)

    y <- httr::PUT(path, body = list(name = name), encode = 'json')

    return(httr::status_code(y) == 200)
}

#' @rdname lights
#' @export
get_lights <- function() {
    path <- bridge_url('lights')

    y <- httr::GET(path)
    y <- httr::content(y, 'text', encoding = 'UTF-8')
    y <- jsonlite::fromJSON(y, simplifyVector = FALSE)

    return(y)
}

#' @rdname lights
#' @export
get_light <- function(id) {
    path <- bridge_url('lights', id)

    y <- httr::GET(path)
    y <- httr::content(y, 'text', encoding = 'UTF-8')
    y <- jsonlite::fromJSON(y, simplifyVector = FALSE)

    return(y)
}

#' @rdname lights
#' @export
set_light_state <- function(id, ...) {
    path <- bridge_url('lights', id, 'state')

    y <- httr::PUT(path, body = list(...), encode = 'json')

    return(httr::status_code(y) == 200)
}

#' @rdname lights
#' @export
delete_light <- function(id) {
    path <- bridge_url('lights', id)

    y <- httr::DELETE(path)

    return(httr::status_code(y) == 200)
}
