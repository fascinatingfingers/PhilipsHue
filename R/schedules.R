
#' Hue API: schedules endpoints
#'
#' @param id id of a specific schedule
#' @param ... named parameters describing schedule attributes
#'   (e.g. `name = 'foo'`)
#' @param return_id when creating a new resource, the default is to return
#'   `TRUE` (invisibly) upon success; setting `return_id = TRUE` will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return `TRUE` (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso <https://developers.meethue.com/documentation/schedules-api-0>
#'
#' @name schedules

#' @rdname schedules
#' @export
create_schedule <- function(..., return_id = FALSE) {
    path <- bridge_url('schedules')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname schedules
#' @export
get_schedules <- function() {
    path <- bridge_url('schedules')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname schedules
#' @export
get_schedule <- function(id) {
    path <- bridge_url('schedules', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname schedules
#' @export
set_schedule_attributes <- function(id, ...) {
    path <- bridge_url('schedules', id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname schedules
#' @export
delete_schedule <- function(id) {
    path <- bridge_url('schedules', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
