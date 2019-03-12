
# An R Client for the Philips Hue API

[![pipeline status](https://gitlab.com/fascinatingfingers/philipshue/badges/master/pipeline.svg)](https://gitlab.com/fascinatingfingers/philipshue/pipelines)
[![coverage report](https://gitlab.com/fascinatingfingers/philipshue/badges/master/coverage.svg)](https://gitlab.com/fascinatingfingers/philipshue/commits/master)

---

The [Philips Hue API](https://www.developers.meethue.com/) provides fine-grained
control over your Hue lighting system, and the PhilipsHue package for R aims to
simplify making requests to the API and processing responses.

## Installation

Install directly from GitHub with:

```{r}
devtools::install_github('fascinatingfingers/PhilipsHue')
```

## Getting started

To connect to your Hue Bridge, you'll need to discover the Bridge IP address and
create a `username`. Read the
[Getting Started](https://www.developers.meethue.com/documentation/getting-started)
guide for more detailed instructions.

You can then save these secrets to the session options with the following.

```{r}
PhilipsHue::set_bridge_credentials(
    ip = '999.999.999.999',
    username = '<Philips Hue username>'
)
```

Here are some examples of what you can do once you've set your credentials.

```{r}
library(tidyverse)
library(PhilipsHue)

# # Delete and re-add all lights (Warning!)
# map_lgl(names(get_lights()), delete_light)
# search_for_new_lights()
# get_new_lights()

# Rename a light
rename_light(1, 'Table lamp')

# Create a room
group_id <- create_group(
    name = 'Living room',
    type = 'Room',
    class = guess_room_class('Living room'),
    lights = I(1:4),
    return_id = TRUE
)

# Define a scene
scene_id <- create_scene('Default', I(1:4), return_id = TRUE)
map_lgl(1:4, set_scene_lightstate, scene_id = scene_id, on = TRUE, bri = 134, ct = 316)

# Configure a switch to turn on the scene
sensor_id <- 42
create_rule(
    name = 'Living room - Default',
    conditions = list(
        condition(sprintf('/sensors/%s/state/buttonevent', sensor_id), 'eq', '1002'),
        condition(sprintf('/sensors/%s/state/lastupdated', sensor_id), 'dx')
    ),
    actions = list(
        action(sprintf('/groups/%s/action', group_id), 'PUT', scene = scene_id)
    )
),
```
