source("./src/class/Sprite.R")

load_sprites <- function() {
  asset_filenames <- list.files("./assets", pattern = ".png")
  assets <- lapply(
    paste0("./assets/", asset_filenames),
    function(x) Sprite$new(x)
  )
  names(assets) <- gsub(".png", "", asset_filenames)

  return(assets)
}
