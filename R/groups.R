
#' Hue API: groups endpoints
#'
#' @param id id of a specific group, an integer
#' @param ... named parameters describing group attributes or state
#'   (e.g. \code{name = 'foo'}; \code{on = TRUE})
#' @param return_id if \code{TRUE} returns the ID of the newly created object
#'   rather than \code{invisible(TRUE)}
#'
#' @return Requests with side effects invisibly return \code{TRUE} upon success.
#'   GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/groups-api}
#'
#' @name groups

#' @rdname groups
#' @export
create_group <- function(..., return_id = FALSE) {
    path <- bridge_url('groups')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname groups
#' @export
get_groups <- function() {
    path <- bridge_url('groups')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname groups
#' @export
get_group <- function(id) {
    path <- bridge_url('groups', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname groups
#' @export
set_group_attributes <- function(id, ...) {
    path <- bridge_url('groups', id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname groups
#' @export
set_group_state <- function(id, ...) {
    path <- bridge_url('groups', id, 'action')
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname groups
#' @export
delete_group <- function(id) {
    path <- bridge_url('groups', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
