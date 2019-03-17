
#' Hue API: rules endpoints
#'
#' @param id id of a specific rule
#' @param name name to assign to the rule
#' @param conditions a list of conditions (e.g. the result of a call to
#'   [condition()] )
#' @param actions a list of actions (e.g. the result of a call to
#'   [action()] )
#' @param return_id when creating a new resource, the default is to return
#'   `TRUE` (invisibly) upon success; setting `return_id = TRUE` will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return `TRUE` (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso <https://developers.meethue.com/documentation/rules-api>
#'
#' @name rules

#' @rdname rules
#' @export
create_rule <- function(name, conditions, actions, return_id = FALSE) {
    path <- bridge_url('rules')
    y <- httr::POST(
        path,
        body = list(name = name, conditions = conditions, actions = actions),
        encode = 'json'
    )
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
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

#' Rule Helpers
#'
#' Defining rules can become quite verbose, and it can be tricky to prepare the
#' proper list structure for the POST or PUT request. These functions simplify
#' things a bit and provide a leaner, more semantic interface.
#'
#' @param address path to attribute or resource
#' @param operator one of: eq, gt, lt, dx, ddx, stable, not stable, in, not in
#' @param value the value a condition will compare against
#' @param method the HTTP method used to send the body to the given address
#' @param ... named parameters to include in action body
#'
#' @return Returns a list-like structure suitable for [create_rule()]
#'   or [set_rule_attributes()].
#'
#' @name rule_helpers

#' @rdname rule_helpers
#' @export
condition <- function(address, operator, value = NULL) {
    y <- list(address = address, operator = operator)
    if (!is.null(value)) y$value <- as.character(value)
    return(y)
}


#' @rdname rule_helpers
#' @export
action <- function(address, method, ...) {
    list(address = address, method = method, body = list(...))
}
