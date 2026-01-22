terminal_player <- function(minefield) {
  is_alive <- TRUE
  minefield$print()

  while (is_alive) {
    print("Enter x of tile to probe")
    x <- readline() |> as.numeric()
    print("Enter y of tile to probe")
    y <- readline() |> as.numeric()

    probed_tile <- minefield$probe_tile(x, y)
    if (probed_tile == -1) {
      print("BOOM")
      is_alive <- FALSE
    }
    minefield$print()
  }

  minefield$print_bare()
}
