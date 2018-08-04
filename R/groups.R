
#' Hue API: groups endpoints
#'
#' @param id id of a specific group
#' @param ... named parameters describing group attributes or state
#'   (e.g. \code{name = 'foo'}; \code{on = TRUE})
#' @param return_id when creating a new resource, the default is to return
#'   \code{TRUE} (invisibly) upon success; setting \code{return_id = TRUE} will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return \code{TRUE} (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
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
