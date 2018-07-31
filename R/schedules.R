
#' Hue API: schedules endpoints
#'
#' @param id id of a specific schedule, an integer
#' @param ... named parameters describing schedule attributes
#'   (e.g. \code{name = 'foo'})
#' @param return_id if \code{TRUE} returns the ID of the newly created object
#'   rather than \code{invisible(TRUE)}
#'
#' @return Requests with side effects invisibly return \code{TRUE} upon success.
#'   GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/schedules-api-0}
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
