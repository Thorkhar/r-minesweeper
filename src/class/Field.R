source("src/class/Tile.R")

Field <- R6Class(
  "Field",
  public = list(
    initialize = function(width, height) {
      private$.width <- width
      private$.height <- height
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
      for (y in seq_len(private$.height)) {
        for (x in seq_len(private$.width)) {
          mines_near <- 0
          offsets <- expand.grid(dx = -1:1, dy = -1:1) |>
            mutate(dx = dx + x, dy = dy + y) |>
            filter(
              !(dx == x & dy == y),
              between(dx, 1, private$.width),
              between(dy, 1, private$.height)
            )

          for (idx in seq_len(nrow(offsets))) {
            if (private$.tiles[offsets$dx[idx], offsets$dy[idx]][[1]]$is_mine) {
              mines_near <- mines_near + 1
            }
          }
          private$.tiles[x, y][[1]]$mines_near <- mines_near
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
    .tiles = NULL
  ),
  active = list(
    width = function() private$.width,
    height = function() private$.height,
    area = function() private$.area,
    tiles = function() private$.tiles
  )
)
