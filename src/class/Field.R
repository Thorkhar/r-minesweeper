source("src/class/Tile.R")

Field <- R6Class(
  "Field",
  public = list(
    initialize = function(width, height) {
      private$.width <- width
      private$.height <- height
      private$.area <- width * height
      private$.tiles <- self$generate_field()
      self$calculate_near_mines()
    },
    generate_field = function() {
      tiles <- sapply(
        seq_len(private$.height),
        function(i) {
          return(
            sapply(
              seq_len(private$.width),
              function(j) Tile$new(x = j, y = i, is_mine = rbinom(1, 1, 0.1))
            )
          )
        }
      )

      return(tiles)
    },
    calculate_near_mines = function() {
      for (i in seq_len(private$.height)) {
        for (j in seq_len(private$.width)) {
          mines_near <- 0
          lookup_tiles <- list(
            c(i + 1, j),
            c(i - 1, j),
            c(i, j + 1),
            c(i, j - 1),
            c(i - 1, j - 1),
            c(i + 1, j + 1),
            c(i - 1, j + 1),
            c(i + 1, j - 1)
          )
          for (crds in lookup_tiles) {
            if (crds[1] < 1 || crds[2] < 1 || crds[1] > private$.width || crds[2] > private$.height) next
            tile <- private$.tiles[crds[1], crds[2]][[1]]
            mines_near <- mines_near + tile$is_mine
          }
          private$.tiles[i, j][[1]]$mines_near <- mines_near
        }
      }
    },
    probe_tile = function(x, y) {
      return(private$.tiles[x, y][[1]]$probe())
    },
    flag_tile = function(x, y) {
      return(private$.tiles[x, y][[1]]$flag())
    }
  ),
  private = list(
    .width = NULL,
    .height = NULL,
    .area = NULL,
    .tiles = NULL
  ),
  active = list(
    width = function() private$.width,
    height = function() private$.height,
    area = function() private$.area,
    tiles = function() private$.tiles
  )
)
