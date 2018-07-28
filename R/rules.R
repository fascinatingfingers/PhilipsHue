
#' Hue API: rules endpoints
#'
#' @param id id of a specific rule, an integer
#' @param name a name to give the rule
#' @param conditions a list of conditions (e.g. the result of a call to
#'   \code{\link{condition}} )
#' @param actions a list of conditions (e.g. the result of a call to
#'   \code{\link{action}} )
#'
#'
#' @return Requests with side effects invisibly return \code{TRUE} upon success.
#'   GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/rules-api}
#'
#' @name rules

#' @rdname rules
#' @export
create_rule <- function(name, conditions, actions) {
    path <- bridge_url('rules')
    y <- httr::POST(
        path,
        body = list(name = name, conditions = conditions, actions = actions),
        encode = 'json'
    )
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname rules
#' @export
get_rules <- function() {
    path <- bridge_url('rules')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname rules
#' @export
get_rule <- function(id) {
    path <- bridge_url('rules', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname rules
#' @export
set_rule_attributes <- function(id, name = NULL, conditions = NULL, actions = NULL) {
    path <- bridge_url('rules', id)
    body <- list()
    if (!is.null(name)) y$name <- as.character(name)
    if (!is.null(conditions)) y$conditions <- as.character(conditions)
    if (!is.null(actions)) y$actions <- as.character(actions)
    y <- httr::PUT(path, body = body, encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname rules
#' @export
delete_rule <- function(id) {
    path <- bridge_url('rules', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
