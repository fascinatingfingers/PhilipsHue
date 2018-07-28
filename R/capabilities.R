
#' Get Capabilities
#'
#' @return Returns a list structure with the capabilities of bridges resources
#'
#' @export
get_capabilities <- function() {
    path <- bridge_url('capabilities')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}
