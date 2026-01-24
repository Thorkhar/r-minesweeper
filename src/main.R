library(dplyr)
library(R6)

cfg <- jsonlite::fromJSON("settings.json")

source("src/class/Field.R")
source("src/terminal_player.R")

minefield <- Field$new(width = cfg$field_width, height = cfg$field_height)

terminal_player(minefield)
