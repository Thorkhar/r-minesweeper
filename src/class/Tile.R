Tile <- R6Class(
  "Tile",
  public = list(
    initialize = function(x, y, is_mine) {
      private$.x <- x
      private$.y <- y
      private$.is_mine <- is_mine
    },
    probe = function() {
      #' Probes the tile to check whether it's a mine or not
      #'
      #' If the tile is flag or already probed, return 0 immediately.
      #' Else return 1 if probe is successful and tile is not a mine, or return
      #' -1 if the tile is a mine.
      #' @return Integer probe result
      if (private$.is_flagged || private$.is_probed) {
        return(0)
      }

      private$.is_probed <- TRUE
      if (private$.is_mine) {
        return(-1)
      } else {
        return(1)
      }
    },
    flag = function() {
      #' Flags a tile to prevent probing it
      #'
      #' If a tile is already probed, it cannot be flagged.
      #' @return Boolean indicating the tile's current flag state
      if (private$.is_probed) {
        return(private$.is_flagged)
      }

      private$.is_flagged <- !private$.is_flagged
      return(private$.is_flagged)
    },
    mines_near = NULL
  ),
  private = list(
    .x = NULL,
    .y = NULL,
    .is_mine = NULL,
    .is_probed = FALSE,
    .is_flagged = FALSE
  ),
  active = list(
    x = function() private$.x,
    y = function() private$.y,
    is_mine = function() private$.is_mine,
    is_probed = function() private$.is_probed,
    is_flagged = function() private$.is_flagged
  )
)
