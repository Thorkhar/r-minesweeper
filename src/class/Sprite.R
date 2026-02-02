Sprite <- R6Class(
  "Sprite",
  public = list(
    initialize = function(file_path) {
      private$.img <- readPNG(file_path)
      private$.grob <- rasterGrob(private$.img, interpolate = TRUE)
    }
  ),
  private = list(
    .img = NULL,
    .grob = NULL
  ),
  active = list(
    grob = function() private$.grob
  )
)
