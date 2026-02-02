source("./src/class/Tile.R")

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
      #' Generate minefield layout
      #'
      #' Generates a width x height sized minefield
      #' Assigns tiles as a mine randomly using a Bernoulli trial
      #' @return Matrix containing Tile objects
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
      #' Calculate number of near mines for each non-mine tile
      #'
      #' Loops over each individual tile in the minefield and counts the number
      #' of mines in the 8 tiles surrounding the target tile.
      #' @return NULL
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
      return(NULL)
    },
    probe_tile = function(x, y) {
      #' Probes a specific tile
      #'
      #' Probes a tile in the minefield specified by the x and y coordinatei in
      #' the input parameters. If the probe is successful and the tile has no
      #' surrounding mines, activate the probe-spread feature.
      #' @param x Integer x coordinate of target tile
      #' @param y Integer y coordinate of target tile
      #' @return Probe result from the Tile's probe method
      probe_res <- private$.tiles[x, y][[1]]$probe()
      if (probe_res == 1 && private$.tiles[x, y][[1]]$mines_near == 0) {
        self$probe_spread(x, y)
      } else if (probe_res == -1) {
        private$.is_alive <- FALSE
      }
      return(probe_res)
    },
    flag_tile = function(x, y) {
      #' Flag a specific tile
      #'
      #' Flagging prevents a tile from being probed
      #' @param x Integer x coordinate of target tile
      #' @param y Integer y coordinate of target tile
      #' @return Flag result from Tile's flag method
      return(private$.tiles[x, y][[1]]$flag())
    },
    probe_spread = function(origin_x, origin_y) {
      #' Spread probing of tiles
      #'
      #' If a probed tile has no surround mines, probe it's direct neighbours as
      #' well. Repeat this process until a tile with surrounding mines is found.
      #' @param origin_x Integer x coordinate of target tile
      #' @param origin_y Integer y coordinate of target tile
      #' @return NULL
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
      return(NULL)
    },
    as_df = function() {
      #' Convert Matrix Tile field to dataframe representation
      #'
      #' Each row in the dataframe represents a tile, indicated by its x and y
      #' coordinate. Additionally has columns indicating whether the tile is
      #' probed, flagged a mine, and how many mines are near. Can be used for
      #' visualizing the field in plotting libraries.
      #' @return DataFrame representation of Matrix Tile field
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
    .tiles = NULL,
    .is_alive = TRUE
  ),
  active = list(
    width = function() private$.width,
    height = function() private$.height,
    area = function() private$.area,
    tiles = function() private$.tiles,
    is_alive = function() private$.is_alive
  )
)
