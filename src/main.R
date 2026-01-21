library(ggplot2)
library(R6)

field_width <- 10
field_height <- 10

source("src/class/Field.R")

minefield <- Field$new(width = field_width, height = field_height)
