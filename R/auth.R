
# LOCAL AUTHENTICATION #########################################################

#' Authentication -- local
#'
#' This function helps check and set the necessary environment variables to
#' authenticate to a Hue Bridge on the local network.
#'
#' @param ip the IP address of your Hue Bridge
#' @param username the username with access to your Hue Bridge
#'
#' @return Returns `TRUE` (invisibly) if options were successfully set
#'
#' @seealso <https://www.developers.meethue.com/documentation/getting-started>
#'
#' @export
auth_local <- function(
    ip = Sys.getenv('PHILIPS_HUE_BRIDGE_IP'),
    username = Sys.getenv('PHILIPS_HUE_BRIDGE_USERNAME')
) {

    # Check inputs
    if (!(!is.null(ip) && is.character(ip) && length(ip) == 1L)) {
        stop('`ip` must be a single character value')
    }

    if (!(!is.null(username) && is.character(username) && length(username) == 1L)) {
        stop('`username` must be a single character value')
    }

    if (length(ip) != 1L || !grepl('^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$', ip)) {
        stop('Invalid value for `ip`')
    }

    if (length(username) != 1L || !grepl('^\\w{40}$', username)) {
        stop('Invalid value for `username`')
    }

    # Sanitize, just in case
    ip <- utils::URLencode(ip)
    username <- utils::URLencode(username)

    # Save secrets as environment variables
    Sys.setenv(
        PHILIPS_HUE_BRIDGE_IP = ip,
        PHILIPS_HUE_BRIDGE_USERNAME = username
    )

    # Fin
    return(invisible(TRUE))
}



# REMOTE AUTHENTICATION ########################################################

#' Authentication -- remote
#'
#' TODO
#'
#' @param app_id your app's ID (the name you registered your app under)
#' @param client_id your app's client ID (assigned when you registered your app
#'   with Hue)
#' @param client_secret your app's client secret (assigned when you registered
#'   your app with Hue)
#' @param bridge_id ID of hue bridge you'd like remote access to
#' @param bridge_name name of hue bridge you'd like remote access to
#' @param username TODO
#' @param access_token access token returned from [request_token()] (or a
#'   subsequent [refresh_token()])
#' @param access_token_exp access token expiration time returned from
#'   [request_token()] (or a subsequent [refresh_token()])
#' @param refresh_token refresh token returned from [request_token()] (or a
#'   subsequent [refresh_token()])
#' @param refresh_token_exp refresh token expiration time returned from
#'   [request_token()] (or a subsequent [refresh_token()])
#' @param initial_setup if `TRUE`, and initial app authorization is required,
#'   [auth_remote()] will prompt the user to authorize the app and enter the
#'   auth code manually
#'
#' @return Returns `TRUE` (invisibly) if options were successfully set
#'
#' @seealso <https://developers.meethue.com/develop/hue-api/remote-api-quick-start-guide/>
#'
#' @export
auth_remote <- function(
    app_id = Sys.getenv('PHILIPS_HUE_APP_ID'),
    client_id = Sys.getenv('PHILIPS_HUE_CLIENT_ID'),
    client_secret = Sys.getenv('PHILIPS_HUE_CLIENT_SECRET'),
    bridge_id = Sys.getenv('PHILIPS_HUE_BRIDGE_ID'),
    bridge_name = Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'),
    username = Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME'),
    access_token = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'),
    access_token_exp = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP'),
    refresh_token = Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'),
    refresh_token_exp = Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP'),
    initial_setup = interactive()
) {

    # Check for a username
    if (remote_username_valid(username)) {
        Sys.setenv(PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = as.character(username))

        if (access_token_valid(access_token, access_token_exp)) {
            Sys.setenv(
                PHILIPS_HUE_ACCESS_TOKEN = as.character(access_token),
                PHILIPS_HUE_ACCESS_TOKEN_EXP = as.character(access_token_exp)
            )
            return(invisible(TRUE))
        } else {

            if (client_valid(client_id, client_secret)) {
                Sys.setenv(
                    PHILIPS_HUE_CLIENT_ID = as.character(client_id),
                    PHILIPS_HUE_CLIENT_SECRET = as.character(client_secret)
                )
            } else {
                warning('Unable to obtain username. Check environment variables: PHILIPS_HUE_CLIENT_ID, PHILIPS_HUE_CLIENT_SECRET')
            }

            if (refresh_token_valid(refresh_token, refresh_token_exp)) {
                token <- refresh_token(refresh_token, client_id, client_secret)

                Sys.setenv(
                    PHILIPS_HUE_ACCESS_TOKEN = as.character(token$access_token),
                    PHILIPS_HUE_ACCESS_TOKEN_EXP = as.character(
                        lubridate::now('UTC') +
                        lubridate::seconds(as.numeric(token$access_token_expires_in))
                    ),
                    PHILIPS_HUE_REFRESH_TOKEN = as.character(token$refresh_token),
                    PHILIPS_HUE_REFRESH_TOKEN_EXP = as.character(
                        lubridate::now('UTC') +
                        lubridate::seconds(as.numeric(token$refresh_token_expires_in))
                    )
                )

                return(invisible(TRUE))
            } else {
                warning('Unable to obtain refresh token. Check environment variables: PHILIPS_HUE_REFRESH_TOKEN, PHILIPS_HUE_REFRESH_TOKEN_EXP')
                return(invisible(FALSE))
            }
        }


    # If there isn't a username...
    } else {

        ## and this is an interactive session...
        if (isTRUE(initial_setup) && isTRUE(interactive())) {

            ### then get the user to approve remote access for your app...

            if (app_id_valid(app_id)) {
                Sys.setenv(PHILIPS_HUE_APP_ID = as.character(app_id))
            } else {
                warning('Unable to obtain username. Check environment variable: PHILIPS_HUE_APP_ID')
            }

            if (client_valid(client_id, client_secret)) {
                Sys.setenv(
                    PHILIPS_HUE_CLIENT_ID = as.character(client_id),
                    PHILIPS_HUE_CLIENT_SECRET = as.character(client_secret)
                )
            } else {
                warning('Unable to obtain username. Check environment variables: PHILIPS_HUE_CLIENT_ID, PHILIPS_HUE_CLIENT_SECRET')
            }

            if (bridge_valid(bridge_id, bridge_name)) {
                Sys.setenv(
                    PHILIPS_HUE_BRIDGE_ID = as.character(bridge_id),
                    PHILIPS_HUE_BRIDGE_NAME = as.character(bridge_name)
                )
            } else {
                warning('Unable to obtain username. Check environment variables: PHILIPS_HUE_BRIDGE_ID, PHILIPS_HUE_BRIDGE_NAME')
            }

            if (app_id_valid() && client_valid() && bridge_valid()) {
                message('Visit the following site for an auth code:\n', authorize_at())

                auth_code <- readline('Enter auth code: ')

                token <- request_token(auth_code)

                remote_auth(token$access_token)

                username <- request_app_username(token$access_token)

                Sys.setenv(
                    PHILIPS_HUE_BRIDGE_REMOTE_USERNAME = as.character(username),
                    PHILIPS_HUE_ACCESS_TOKEN = as.character(token$access_token),
                    PHILIPS_HUE_ACCESS_TOKEN_EXP = as.character(
                        lubridate::now('UTC') +
                        lubridate::seconds(as.numeric(token$access_token_expires_in))
                    ),
                    PHILIPS_HUE_REFRESH_TOKEN = as.character(token$refresh_token),
                    PHILIPS_HUE_REFRESH_TOKEN_EXP = as.character(
                        lubridate::now('UTC') +
                        lubridate::seconds(as.numeric(token$refresh_token_expires_in))
                    )
                )

                return(invisible(TRUE))

            } else {

                return(invisible(FALSE))

            }

        ## If this isn't an interactive session...
        } else {

            ### Warn and return FALSE
            warning('Unable to obtain username. Check environment variable: PHILIPS_HUE_BRIDGE_REMOTE_USERNAME')
            return(invisible(FALSE))
        }
    }
}



# REMOTE AUTH SEQUENCE #########################################################

#' Request remote access to user's bridge
#'
#' The first step in the remote auth process is to request remote access to the
#' user's bridge. When they click the generated URL, they will be taken to the
#' Hue website where they can grant remote access to your app. Once approved,
#' Hue will redirect them to the callback URL you provided when you registered
#' your app. This redirect URL will include an authorization code as a query
#' parameter. Retain this code.
#'
#' @param client_id your app's client ID (assigned when you registered your app
#'   with Hue)
#' @param app_id your app's ID (the name you registered your app under)
#' @param bridge_id ID of hue bridge you'd like remote access to
#' @param bridge_name name of hue bridge you'd like remote access to
#' @param state arbitrary data that will be included in the redirected response
#'   (see Details)
#'
#' When `state = NULL` (the default), a random hash will be generated. You can
#' verify this value in the redirected response for an added layer of security.
#'
#' @return Returns a URL to request remote access to a user's bridge.
#'
#' @examples
#' authorize_at('client_id', 'app id', 'bridge_id', 'bridge name')
#'
#' @export
authorize_at <- function(
    client_id = Sys.getenv('PHILIPS_HUE_CLIENT_ID'),
    app_id = Sys.getenv('PHILIPS_HUE_APP_ID'),
    bridge_id = Sys.getenv('PHILIPS_HUE_BRIDGE_ID'),
    bridge_name = Sys.getenv('PHILIPS_HUE_BRIDGE_NAME'),
    state = NULL
) {
    if (is.null(state)) {state <- digest::digest(stats::rnorm(1000))}

    sprintf(
        'https://api.meethue.com/oauth2/auth?clientid=%s&appid=%s&deviceid=%s&devicename=%s&state=%s&response_type=code',
        utils::URLencode(client_id),
        utils::URLencode(app_id),
        utils::URLencode(bridge_id),
        utils::URLencode(bridge_name),
        utils::URLencode(state)
    )
}

#' Request authorization token
#'
#' After you obtain an authorization code (see [authorize_at()]), use the code
#' to obtain an authorization token.
#'
#' @param auth_code authorization code sent to your app's callback URL (see
#'   [authorize_at()])
#' @param client_id your app's client ID (assigned when you registered your app
#'   with Hue)
#' @param client_secret your app's client secret (assigned when you registered
#'   your app with Hue)
#'
#' @return If successful, returns a list with authorization/refresh tokens and
#'   expiration times
#'
#' @export
request_token <- function(
    auth_code = Sys.getenv('PHILIPS_HUE_AUTH_CODE'),
    client_id = Sys.getenv('PHILIPS_HUE_CLIENT_ID'),
    client_secret = Sys.getenv('PHILIPS_HUE_CLIENT_SECRET')
) {
    x <- sprintf(
        'https://api.meethue.com/oauth2/token?code=%s&grant_type=authorization_code',
        utils::URLencode(auth_code)
    )

    res <- httr::POST(x, httr::add_headers(
        Authorization = base64enc::base64encode(charToRaw(paste(client_id, client_secret, sep = ':')))
    ))

    res_status <- tryCatch(
        httr::status_code(res),
        error = function(e) {NA}
    )

    res_content <- tryCatch(
        httr::content(res, as = 'parsed'),
        error = function(e) {list(`httr::content error` = as.character(e))}
    )

    if (res_status %in% 200 && 'access_token' %in% names(res_content)) {
        return(res_content)
    } else {
        stop('Token request faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}

#' Remotely activate a button press on the user's bridge
#'
#' After obtaining an authorization token (see [request_token()]), the token can
#' be used to remotely "press" the button on the Hue bridge as part of a remote
#' authentication sequence.
#'
#' @param token token returned from [request_token()]
#'
#' @return Returns `TRUE` (invisibly) if the call was successful.
#'
#' @export
remote_auth <- function(token = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN')) {
    res <- httr::PUT(
        'https://api.meethue.com/bridge/0/config',
        httr::add_headers(Authorization = paste('Bearer', token)),
        body = list(linkbutton = TRUE),
        encode = 'json'
    )

    res_status <- tryCatch(
        httr::status_code(res),
        error = function(e) {NA}
    )

    if (res_status %in% 200) {
        return(invisible(TRUE))
    } else {
        res_content <- tryCatch(
            httr::content(res, as = 'parsed'),
            error = function(e) {list(`httr::content error` = as.character(e))}
        )

        stop('Remote button press faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}

#' Whitelist your app and receive a username
#'
#' @param token token returned from [request_token()]
#' @param app_id your app's ID (the name you registered your app under)
#'
#' @return Adds your app ID to the list of whitelisted apps on the user's bridge
#'   and returns a username that can be used to authenticate future requests.
#'
#' @export
request_app_username <- function(
    token = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'),
    app_id = Sys.getenv('PHILIPS_HUE_APP_ID')
) {
    res <- httr::POST(
        'https://api.meethue.com/bridge',
        httr::add_headers(Authorization = paste('Bearer', token)),
        body = list(devicetype = app_id),
        encode = 'json'
    )

    res_status <- tryCatch(
        httr::status_code(res),
        error = function(e) {NA}
    )

    res_content <- tryCatch(
        httr::content(res, as = 'parsed'),
        error = function(e) {list(`httr::content error` = as.character(e))}
    )

    if (res_status %in% 200 && 'success' %in% names(res_content[[1]])) {
        return(res_content[[1]]$success$username)
    } else {
        stop('Token request faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}

#' Refresh access token
#'
#' Tokens received by calling [request_token()] expire but can be refreshed by
#' a simple call to [refresh_token()].
#'
#' @param refresh_token refresh token returned from [request_token()] (or
#'   a subsequent [refresh_token()])
#' @param client_id your app's client ID (assigned when you registered your app
#'   with Hue)
#' @param client_secret your app's client secret (assigned when you registered
#'   your app with Hue)
#'
#' @return If successful, returns a list with authorization/refresh tokens and
#'   expiration times
#'
#' @export
refresh_token <- function(
    refresh_token = Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'),
    client_id = Sys.getenv('PHILIPS_HUE_CLIENT_ID'),
    client_secret = Sys.getenv('PHILIPS_HUE_CLIENT_SECRET')
) {
    res <- httr::POST(
        'https://api.meethue.com/oauth2/refresh?grant_type=refresh_token',
        httr::add_headers(
            Authorization = base64enc::base64encode(charToRaw(paste(client_id, client_secret, sep = ':')))
        ),
        body = list(refresh_token = refresh_token),
        encode = 'form'
    )

    res_status <- tryCatch(
        httr::status_code(res),
        error = function(e) {NA}
    )

    res_content <- tryCatch(
        httr::content(res, as = 'parsed'),
        error = function(e) {list(`httr::content error` = as.character(e))}
    )

    if (res_status %in% 200) {
        return(res_content)
    } else {
        stop('Token refresh faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}



# VALIDATORS ###################################################################

app_id_valid <- function(
    app_id = Sys.getenv('PHILIPS_HUE_APP_ID')
) {
    (methods::hasArg(app_id) || !is.null(app_id)) &&
    is.character(app_id) &&
    length(app_id) == 1L &&
    grepl('\\w', app_id)
}

client_valid <- function(
    client_id = Sys.getenv('PHILIPS_HUE_CLIENT_ID'),
    client_secret = Sys.getenv('PHILIPS_HUE_CLIENT_SECRET')
) {
    (methods::hasArg(client_id) || !is.null(client_id)) &&
    is.character(client_id) &&
    length(client_id) == 1L &&
    grepl('^\\w+$', client_id) &&

    (methods::hasArg(client_secret) || !is.null(client_secret)) &&
    is.character(client_secret) &&
    length(client_secret) == 1L &&
    grepl('^\\w+$', client_secret)
}

remote_username_valid <- function(
    username = Sys.getenv('PHILIPS_HUE_BRIDGE_REMOTE_USERNAME')
) {
    (methods::hasArg(username) || !is.null(username)) &&
    is.character(username) &&
    length(username) == 1L &&
    grepl('^[-A-Za-z0-9]+$', username)
}

access_token_valid <- function(
    access_token = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN'),
    access_token_exp = Sys.getenv('PHILIPS_HUE_ACCESS_TOKEN_EXP')
) {
    (methods::hasArg(access_token) || !is.null(access_token)) &&
    is.character(access_token) &&
    length(access_token) == 1L &&
    grepl('^\\w+$', access_token) &&
    (methods::hasArg(access_token_exp) || !is.null(access_token_exp)) &&
    length(access_token_exp) == 1L &&
    (tryCatch(
        as.POSIXct(access_token_exp, 'UTC'),
        error = function(e) {lubridate::now('UTC') - lubridate::days(3)}
    ) > lubridate::now('UTC')) %in% TRUE
}

refresh_token_valid <- function(
    refresh_token = Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN'),
    refresh_token_exp = Sys.getenv('PHILIPS_HUE_REFRESH_TOKEN_EXP')
) {
    (methods::hasArg(refresh_token) || !is.null(refresh_token)) &&
    is.character(refresh_token) &&
    length(refresh_token) == 1L &&
    grepl('^\\w+$', refresh_token) &&
    (methods::hasArg(refresh_token_exp) || !is.null(refresh_token_exp)) &&
    length(refresh_token_exp) == 1L &&
    (tryCatch(
        as.POSIXct(refresh_token_exp, 'UTC'),
        error = function(e) {lubridate::now('UTC') - lubridate::days(3)}
    ) > lubridate::now('UTC')) %in% TRUE
}

bridge_valid <- function(
    bridge_id = Sys.getenv('PHILIPS_HUE_BRIDGE_ID'),
    bridge_name = Sys.getenv('PHILIPS_HUE_BRIDGE_NAME')
) {
    (methods::hasArg(bridge_id) || !is.null(bridge_id)) &&
    is.character(bridge_id) &&
    length(bridge_id) == 1L &&
    grepl('^\\w+$', bridge_id) &&

    (methods::hasArg(bridge_name) || !is.null(bridge_name)) &&
    is.character(bridge_name) &&
    length(bridge_name) == 1L &&
    grepl('\\w', bridge_name)
}
