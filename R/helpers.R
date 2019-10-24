
#' Process HTTP Responses
#'
#' This function performs basic error handling and parsing of `httr`
#'   [httr::response()] objects returned from Hue API requests.
#'
#' @param x an `httr` [httr::response()] object
#' @param ... parameters passed to [jsonlite::fromJSON()]
#'
#' @return Returns response content, parsed into an R-friendly list.
#'
process_httr_response <- function(x, ...) {
    if (!methods::is(x, 'response')) {
        stop('Input `x` must be an httr response object', call. = FALSE)
    }

    if (httr::http_type(x) != 'application/json') {
        stop('API did not return json', call. = FALSE)
    }

    y <- httr::content(x, 'parsed', encoding = 'UTF-8', ...)

    if (httr::http_error(x)) {
        stop(
            sprintf(
                'API request failed [%s]\n%s',
                httr::status_code(x),
                yaml::as.yaml(y)
            ),
            call. = FALSE
        )
    }

    api_errors <- subset(y, purrr::map_lgl(y, ~ identical(names(.), 'error')))

    if (length(api_errors) > 0) {
        stop(
            sprintf(
                'API request returned errors\n%s',
                yaml::as.yaml(api_errors)
            ),
            call. = FALSE
        )
    }

    return(y)
}
