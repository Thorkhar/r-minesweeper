Tile <- R6Class(
  "Tile",
  public = list(
    initialize = function(x, y, is_mine) {
      private$.x <- x
      private$.y <- y
      private$.is_mine <- is_mine
    },
    probe = function() {
      if (!private$.is_probed) {
        private$.is_probed <- TRUE
        return(1)
      } else {
        return(0)
      }
    },
    flag = function() {
      private$.is_flagged <- !private$.is_flagged
      return(NULL)
    }
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
    is_probed = function() private$.is_probed
  )
)
