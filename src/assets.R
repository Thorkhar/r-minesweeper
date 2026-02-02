load_assets <- function() {
  asset_filenames <- list.files("./assets", pattern = ".png")
  assets <- lapply(
    asset_filenames,
    function(filename) {
      return(
        readPNG(paste0("./assets/", filename)) |>
          rasterGrob(interpolate = TRUE)
      )
    }
  )
  names(assets) <- gsub(".png", "", asset_filenames)

  return(assets)
}
