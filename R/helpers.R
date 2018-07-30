
#' Process HTTP Responses
#'
#' This function performs basic error handling and parsing of \code{httr}
#'   \code{\link[httr]{response}} objects.
#'
#' @param x an \code{httr} \code{\link[httr]{response}} object
#' @param ... parameters passed to \code{\link[jsonlite]{fromJSON}}
#'
#' @return Returns the content of the response, parsed into an R-friendly list.
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

#' Hue Bridge Authentication
#'
#' These functions are used primarily for their side effects -- specifically to
#' set (or reset) Hue Bridge authentication secrets stored in
#' \code{options()$PhilipsHue}.
#'
#' @param ip the IP address of your Hue Bridge
#' @param username the username with access to your Hue Bridge
#'
#' @return Returns \code{TRUE} (invisibly) if a connection to the Hue Bridge was
#'   successful.
#'
#' @seealso \url{https://www.developers.meethue.com/documentation/getting-started}
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

    # Attempt to get bridge configuration
    message('Attempting to connect to bridge...')
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
reset_bridge_credentials <- function() {options(PhilipsHue = list())}

#' Create a Hue API endpoint URL
#'
#' @param ... strings to append to the root URL
#'
#' @return Returns a complete URL for the API endpoint
#'
#' @seealso \code{\link{set_bridge_credentials}}
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

#' Rule Helpers
#'
#' @param address path to attribute or resource
#' @param operator one of: eq, gt, lt, dx, ddx, stable, not stable, in, not in
#' @param value the value a condition will compare against
#' @param method the HTTP method used to send the body to the given address
#' @param ... named parameters to include in action body
#'
#' @return Returns a list-like structure suitable for \code{\link{create_rule}}
#'   or \code{\link{set_rule_attributes}}
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

#' Configure Built-In Daylight Sensor
#'
#' @param lat latitude
#' @param lon logitude
#' @param sunriseoffset "daylight" begins \code{sunriseoffset} minutes after
#'   sunrise
#' @param sunsetoffset "daylight" ends \code{sunsetoffset} minutes after sunset
#' @param id ID of the daylight sensor
#'
#' @return Returns \code{TRUE} (invisibly) uppon success
#'
#' @export
configure_daylight_sensor <- function(lat, lon, sunriseoffset = 30, sunsetoffset = -30, id = 1) {
    set_sensor_config(
        id = id,
        lat = ifelse(
            lat < 0,
            sprintf('%03.4fS', abs(lat)),
            sprintf('%03.4fN', abs(lat))
        ),
        long = ifelse(
            lon < 0,
            sprintf('%03.4fW', abs(lon)),
            sprintf('%03.4fE', abs(lon))
        ),
        sunriseoffset = sunriseoffset,
        sunsetoffset = sunsetoffset
    )
}
