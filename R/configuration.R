
# TODO create_user

# TODO delete_user

# TODO get_configuration

# TODO set_configuration_attributes

#' Get Full State
#'
#' @return Returns a list structure with the current state of all devices and
#'   resources.
#'
#' @export
get_state <- function() {
    state <- process_httr_response(httr::GET(bridge_url()))

    for (scene in names(state$scenes)) {
        state$scenes[[scene]]$lightstates <- get_scene(scene)$lightstates
    }
    rm(scene)

    return(state)
}
