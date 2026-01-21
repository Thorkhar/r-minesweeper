library(ggplot2)
library(R6)

field_width <- 10
field_height <- 10

source("src/class/Field.R")

minefield <- Field$new(width = field_width, height = field_height)

terminal_field <- ""
for (i in seq_len(minefield$height)) {
  for (j in seq_len(minefield$width)) {
    if (minefield$tiles[i, j][[1]]$is_mine) {
      terminal_field <- paste0(terminal_field, "|X")
    } else {
      terminal_field <- paste0(terminal_field, "| ")
    }
  }
  terminal_field <- paste0(terminal_field, "|\n")
}

cat(terminal_field)
