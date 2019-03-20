
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
#' remote_auth_request_url('client_id', 'app id', 'bridge_id', 'bridge name')
#'
#' @export
remote_auth_request_url <- function(
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
#' After you obtain an authorization code (see [remote_auth_request_url()]), use
#' the code to obtain an authorization token.
#'
#' @param auth_code authorization code sent to your app's callback URL
#'   (see [remote_auth_request_url()])
#' @param client_id your app's client ID (assigned when you registered your app
#'   with Hue)
#' @param client_secret your app's client secret (assigned when you registered
#'   your app with Hue)
#'
#' @return If successful, returns a list with authorization/refresh tokens and
#'   expiration times
#'
#' @export
token <- function(
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
        error = function(e) {list()}
    )

    if (res_status %in% 200 && 'access_token' %in% names(res_content)) {
        return(res_content$access_token)
    } else {
        stop('Token request faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}

#' Remotely activate a button press on the user's bridge
#'
#' After obtaining an authorization token (see [token()]), the token can be
#' used to remotely "press" the button on the Hue bridge as part of a remote
#' authentication sequence.
#'
#' @param token token returned from [token()]
#'
#' @return Returns `TRUE` (invisibly) if the call was successful.
#'
#' @export
remote_button_press <- function(token) {
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
            error = function(e) {list()}
        )

        stop('Remote button press faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}



#' Whitelist your app and receive a username
#'
#' @param token token returned from [token()]
#' @param app_id your app's ID (the name you registered your app under)
#'
#' @return Adds your app ID to the list of whitelisted apps on the user's bridge
#'   and returns a username that can be used to authenticate future requests.
#'
#' @export
app_username <- function(token, app_id = Sys.getenv('PHILIPS_HUE_APP_ID')) {
    res <- httr::POST(
        'https://api.meethue.com/bridge/',
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
        error = function(e) {list()}
    )

    if (res_status %in% 200 && 'success' %in% names(res_content[[1]])) {
        return(res_content[[1]]$success$username)
    } else {
        stop('Token request faild with status code: ', res_status, ':\n', yaml::as.yaml(res_content))
    }
}
