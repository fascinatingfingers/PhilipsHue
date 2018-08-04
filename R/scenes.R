
#' Hue API: scenes endpoints
#'
#' @param id,scene_id id of a specific scene
#' @param light_id id of a specific light in the scene
#' @param name name to assign to the scene
#' @param lights vector of light IDs included in the scene
#' @param recycle logical indicating whether the scene can be automatically
#'   deleted by the bridge
#' @param transitiontime duration (in milliseconds) of the scene transition
#' @param ... named parameters describing scene attributes or light state
#'   (e.g. \code{name = 'foo'}; \code{on = TRUE})
#' @param return_id when creating a new resource, the default is to return
#'   \code{TRUE} (invisibly) upon success; setting \code{return_id = TRUE} will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return \code{TRUE} (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso \url{https://developers.meethue.com/documentation/scenes-api}
#'
#' @name scenes

#' @rdname scenes
#' @export
create_scene <- function(name, lights, recycle = TRUE, transitiontime = 4, return_id = FALSE) {
    path <- bridge_url('scenes')
    y <- httr::POST(
        path,
        body = list(
            name = name,
            lights = lights,
            recycle = recycle,
            transitiontime = transitiontime
        ),
        encode = 'json'
    )
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname scenes
#' @export
get_scenes <- function() {
    path <- bridge_url('scenes')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname scenes
#' @export
get_scene <- function(id) {
    path <- bridge_url('scenes', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname scenes
#' @export
set_scene_attributes <- function(id, ...) {
    path <- bridge_url('scenes', id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname scenes
#' @export
set_scene_lightstate <- function(scene_id, light_id, ...) {
    path <- bridge_url('scenes', scene_id, 'lightstates', light_id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname scenes
#' @export
delete_scene <- function(id) {
    path <- bridge_url('scenes', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}
