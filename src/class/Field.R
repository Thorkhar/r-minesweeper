source("src/class/Tile.R")

Field <- R6Class(
  "Field",
  public = list(
    initialize = function(width, height) {
      private$.width <- width
      private$.height <- height
      private$.area <- width * height
      private$.tiles <- self$generate_field()
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
