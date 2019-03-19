
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
#' @param device_name name of hue bridge you'd like remote access to
#' @param state arbitrary data that will be included in the redirected response
#'   (see Details)
#'
#' When `state = NULL` (the default), a UUID will be generated. You can verify
#' this value in the redirected response for an added layer of security.
#'
#' @return Returns a URL to request remote access to a user's bridge.
#'
#' @examples
#' remote_auth_request_url('client_id', 'app id', 'device_id', 'device name')
#'
#' @export
remote_auth_request_url <- function(client_id, app_id, bridge_id, device_name, state = NULL) {
    if (is.null(state)) {state <- gsub('-', '', uuid::UUIDgenerate())}

    sprintf(
        'https://api.meethue.com/oauth2/auth?clientid=%s&appid=%s&deviceid=%s&devicename=%s&state=%s&response_type=code',
        utils::URLencode(client_id),
        utils::URLencode(app_id),
        utils::URLencode(bridge_id),
        utils::URLencode(device_name),
        utils::URLencode(state)
    )
}
