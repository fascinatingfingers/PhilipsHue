
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

    # If successful, set options...
    if (config$status_code == 200) {

        message('...success!')

        config <- httr::content(config, 'text', encoding = 'UTF-8')
        config <- jsonlite::fromJSON(config, simplifyVector = FALSE)

        options(PhilipsHue = list(base_url = base_url, config = config))

        return(invisible(TRUE))

    # ...otherwise throw an error
    } else {

        stop('Unable to connect to bridge')

    }
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
