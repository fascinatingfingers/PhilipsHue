
#' Hue API: resourcelinks endpoints
#'
#' @param id id of a specific resourcelink
#' @param ... named parameters describing resourcelink attributes
#'   (e.g. \code{name = 'foo'})
#' @param return_id when creating a new resource, the default is to return
#'   \code{TRUE} (invisibly) upon success; setting \code{return_id = TRUE} will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return \code{TRUE} (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/resourcelinks-api}
#'
#' @name resourcelinks

#' @rdname resourcelinks
#' @export
create_resourcelink <- function(..., return_id = FALSE) {
    path <- bridge_url('resourcelinks')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname resourcelinks
#' @export
get_resourcelinks <- function() {
    path <- bridge_url('resourcelinks')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname resourcelinks
#' @export
get_resourcelink <- function(id) {
    path <- bridge_url('resourcelinks', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname resourcelinks
#' @export
set_resourcelink_attributes <- function(id, ...) {
    path <- bridge_url('resourcelinks', id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname resourcelinks
#' @export
delete_resourcelink <- function(id) {
    path <- bridge_url('resourcelinks', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
