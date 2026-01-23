convert_field_to_df <- function(tile_field) {
  field_coords <- expand.grid(
    x = seq_len(ncol(tile_field)),
    y = seq_len(nrow(tile_field))
  ) |>
    rowwise() |>
    mutate(
      is_probed = tile_field[x, y][[1]]$is_probed,
      is_flagged = tile_field[x, y][[1]]$is_flagged,
      is_mine = tile_field[x, y][[1]]$is_mine,
      mines_near = tile_field[x, y][[1]]$mines_near
    ) |>
    mutate(
      display_label = case_when(
        is_flagged ~ "F",
        is_probed & is_mine ~ "X",
        is_probed & mines_near > 0 ~ as.character(mines_near),
        TRUE ~ ""
      )
    )

  return(field_coords)
}
