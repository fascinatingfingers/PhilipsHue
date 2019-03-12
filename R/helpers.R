
#' Hue Bridge Authentication
#'
#' These functions are used specifically for their side effects -- namely to
#' set (or reset) Philips Hue API authentication secrets stored in
#' `options()$PhilipsHue`.
#'
#' @param ip the IP address of your Hue Bridge
#' @param username the username with access to your Hue Bridge
#'
#' @return Returns `TRUE` (invisibly) if options were successfully set or
#'   reset.
#'
#' @seealso <https://www.developers.meethue.com/documentation/getting-started>
#'
#' @export
#'
set_bridge_credentials <- function(ip, username) {

    # Check inputs
    if (length(ip) != 1L || !grepl('^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$', ip)) {
        stop('Value for `ip` does not appear to be a valid IP address')
    }

    if (length(username) != 1L || !grepl('^\\w{40}$', username)) {
        stop('Value for `username` does not appear to be a valid Philips Hue API username')
    }

    # Prepare base URL
    base_url <- sprintf('http://%s/api/%s', ip, username)

    # Attempt to get Bridge configuration
    message('Attempting to connect to Bridge...')
    config <- httr::GET(paste(base_url, 'config', sep = '/'))

    # Check for errors, then parse response
    config <- process_httr_response(config)
    message('...success!')

    # Set options
    options(PhilipsHue = list(base_url = base_url, config = config))

    return(invisible(TRUE))
}

#' @rdname set_bridge_credentials
#' @export
reset_bridge_credentials <- function() {
    options(PhilipsHue = list())
    return(invisible(TRUE))
}

#' Create a Hue API endpoint URL
#'
#' This function creates a URL for a Hue API endpoint. The base URL is created
#' using secrets set with a call to [set_bridge_credentials()]. It
#' then appends any user-supplied components, separating each with a "/".
#'
#' @param ... strings to append to the root URL
#'
#' @return Returns a complete URL for the API endpoint.
#'
#' @seealso [set_bridge_credentials()]
#'
#' @export
#'
bridge_url <- function(...) {
    base_url <- getOption('PhilipsHue')$base_url

    if (is.null(base_url)) {
        stop("Don't know how to connect to your Philips Hue Bridge! See `??set_bridge_credentials`")
    }

    return(paste(base_url, ..., sep = '/'))
}

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
