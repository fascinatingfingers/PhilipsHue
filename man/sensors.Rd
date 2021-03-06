% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensors.R
\name{sensors}
\alias{sensors}
\alias{create_sensor}
\alias{search_for_new_sensors}
\alias{get_new_sensors}
\alias{rename_sensor}
\alias{get_sensors}
\alias{get_sensor}
\alias{set_sensor_config}
\alias{set_sensor_state}
\alias{delete_sensor}
\title{Hue API: sensors endpoints}
\usage{
create_sensor(..., return_id = FALSE)

search_for_new_sensors()

get_new_sensors()

rename_sensor(id, name)

get_sensors()

get_sensor(id)

set_sensor_config(id, ...)

set_sensor_state(id, ...)

delete_sensor(id)
}
\arguments{
\item{...}{named parameters describing sensor state (e.g. \code{on = TRUE})}

\item{return_id}{when creating a new resource, the default is to return
\code{TRUE} (invisibly) upon success; setting \code{return_id = TRUE} will
return the ID of the newly created resource instead.}

\item{id}{id of a specific sensor}

\item{name}{name to assign to the sensor}
}
\value{
Requests with side effects return \code{TRUE} (invisibly) upon
success. GET requests return the response content, parsed into a list.
}
\description{
Hue API: sensors endpoints
}
\seealso{
\url{https://developers.meethue.com/documentation/sensors-api}
}
