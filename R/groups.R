
#' Hue API: groups endpoints
#'
#' @param id id of a specific group
#' @param ... named parameters describing group attributes or state
#'   (e.g. `name = 'foo'`; `on = TRUE`)
#' @param return_id when creating a new resource, the default is to return
#'   `TRUE` (invisibly) upon success; setting `return_id = TRUE` will
#'   return the ID of the newly created resource instead.
#'
#' @return Requests with side effects return `TRUE` (invisibly) upon
#'   success. GET requests return the response content, parsed into a list.
#'
#' @seealso <https://developers.meethue.com/documentation/groups-api>
#'
#' @name groups

#' @rdname groups
#' @export
create_group <- function(..., return_id = FALSE) {
    path <- bridge_url('groups')
    y <- httr::POST(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    if (return_id) {
        return(as.character(unlist(y)))
    } else {
        return(invisible(TRUE))
    }
}

#' @rdname groups
#' @export
get_groups <- function() {
    path <- bridge_url('groups')
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname groups
#' @export
get_group <- function(id) {
    path <- bridge_url('groups', id)
    y <- httr::GET(path)
    y <- process_httr_response(y)
    return(y)
}

#' @rdname groups
#' @export
set_group_attributes <- function(id, ...) {
    path <- bridge_url('groups', id)
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname groups
#' @export
set_group_state <- function(id, ...) {
    path <- bridge_url('groups', id, 'action')
    y <- httr::PUT(path, body = list(...), encode = 'json')
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' @rdname groups
#' @export
delete_group <- function(id) {
    path <- bridge_url('groups', id)
    y <- httr::DELETE(path)
    y <- process_httr_response(y)
    return(invisible(TRUE))
}

#' Guess Room Class
#'
#' Every new room must be assigned a room "class" (Living room, Kitchen, etc).
#' This function attempts to guess the room class from the room name. For
#' example, a room named "Master Bedroom" would be assigned the class "Bedroom"
#' because it contains a substring match.
#'
#' @param x room name
#'
#' @return Returns a character vector with the single best guess of the room
#'   class of the given room name.
#'
#' @export
guess_room_class <- function(x) {
    room_classes <- c(
        'Living room',
        'Kitchen',
        'Dining',
        'Bedroom',
        'Kids bedroom',
        'Bathroom',
        'Nursery',
        'Recreation',
        'Office',
        'Gym',
        'Hallway',
        'Toilet',
        'Front door',
        'Garage',
        'Terrace',
        'Garden',
        'Driveway',
        'Carport',
        'Other'
    )

    y <- room_classes[sapply(room_classes, grepl, x = x, ignore.case = TRUE)]
    y <- c(y, 'Other')
    y <- y[1]

    if (grepl('^(main|primary)( floor|area)?$', x, ignore.case = TRUE)) {
        y <- 'Living room'
    }

    return(y)
}
