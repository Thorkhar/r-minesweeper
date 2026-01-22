terminal_player <- function(minefield) {
  is_alive <- TRUE
  print_state(minefield)

  while (is_alive) {
    print("Do you want to probe (1) or flag(2)?")
    action <- readline() |> as.numeric()
    print("Enter x of target tile")
    x <- readline() |> as.numeric()
    print("Enter y of target tile")
    y <- readline() |> as.numeric()

    if (action == 1) {
      probed_tile <- minefield$probe_tile(x, y)
      if (probed_tile == -1) {
        print("BOOM")
        is_alive <- FALSE
        break
      }
    } else if (action == 2) {
      flagged_tile <- minefield$flag_tile(x, y)
    } else {
      print("Invalid action")
      next
    }

    print_state(minefield)
  }

  print_bare(minefield)
}

print_state <- function(minefield) {
  terminal_field <- ""
  for (i in seq_len(minefield$height)) {
    for (j in seq_len(minefield$width)) {
      tile <- minefield$tiles[i, j][[1]]
      if (tile$is_probed & tile$is_mine) {
        terminal_field <- paste0(terminal_field, "|X")
      } else if (tile$is_flagged) {
        terminal_field <- paste0(terminal_field, "|F")
      } else if (tile$is_probed && tile$mines_near > 0) {
        terminal_field <- paste0(terminal_field, "|", tile$mines_near)
      } else if (tile$is_probed) {
        terminal_field <- paste0(terminal_field, "| ")
      } else {
        terminal_field <- paste0(terminal_field, "|#")
      }
    }
    terminal_field <- paste0(terminal_field, "|\n")
  }

  cat(terminal_field)
}

print_bare <- function(minefield) {
  terminal_field <- ""
  for (i in seq_len(minefield$height)) {
    for (j in seq_len(minefield$width)) {
      tile <- minefield$tiles[i, j][[1]]
      if (tile$is_mine) {
        terminal_field <- paste0(terminal_field, "|X")
      } else {
        terminal_field <- paste0(terminal_field, "| ")
      }
    }
    terminal_field <- paste0(terminal_field, "|\n")
  }

  cat(terminal_field)
}
