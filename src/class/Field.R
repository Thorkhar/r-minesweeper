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
              function(j) {
                Tile$new(
                  x = j, y = i, is_mine = rbinom(1, 1, cfg$mine_density)
                )
              }
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
      probe_res <- private$.tiles[x, y][[1]]$probe()
      if (probe_res == 1 && private$.tiles[x, y][[1]]$mines_near == 0) {
        self$probe_spread(x, y)
      }
      return(probe_res)
    },
    flag_tile = function(x, y) {
      return(private$.tiles[x, y][[1]]$flag())
    },
    probe_spread = function(origin_x, origin_y) {
      neighbours <- expand.grid(dx = -1:1, dy = -1:1) |>
        mutate(dx = dx + origin_x, dy = dy + origin_y) |>
        filter(
          !(dx == origin_x & dy == origin_y),
          (dx == origin_x | dy == origin_y),
          between(dx, 1, private$.width),
          between(dy, 1, private$.height)
        )

      for (idx in seq_len(nrow(neighbours))) {
        neighbour <- neighbours[idx, ]
        neighbour_tile <- private$.tiles[neighbour$dx, neighbour$dy][[1]]
        if (!neighbour_tile$is_probed &&
          !neighbour_tile$is_mine) {
          self$probe_tile(neighbour_tile$x, neighbour_tile$y)
        }
      }
    },
    as_df = function() {
      df_representation <- expand.grid(
        x = seq_len(ncol(private$.tiles)),
        y = seq_len(nrow(private$.tiles))
      ) |>
        rowwise() |>
        mutate(
          is_probed = private$.tiles[x, y][[1]]$is_probed,
          is_flagged = private$.tiles[x, y][[1]]$is_flagged,
          is_mine = private$.tiles[x, y][[1]]$is_mine,
          mines_near = private$.tiles[x, y][[1]]$mines_near
        )

      return(df_representation)
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
