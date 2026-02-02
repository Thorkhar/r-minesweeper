library(shiny)
library(ggplot2)
library(R6)
library(dplyr)
library(png)
library(grid)

options(shiny.port = 8100, shiny.host = "0.0.0.0")
cfg <- jsonlite::fromJSON("./settings.json")
source("./src/assets.R")
source("./src/class/Field.R")

assets <- load_assets()

ui <- basicPage(
  plotOutput("field", click = "field_click", dblclick = "field_dblclick"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  minefield <- reactiveVal(
    Field$new(width = cfg$field_width, height = cfg$field_height)
  )

  output$field <- renderPlot({
    #' Render minefield on screen using ggplot2 and the DataFrame representation
    #' of the Field class.
    mf <- minefield()
    mf_df <- mf$as_df() |>
      mutate(
        display_label = case_when(
          is_flagged ~ "F",
          is_probed & is_mine ~ "X",
          is_probed & mines_near > 0 ~ as.character(mines_near),
          TRUE ~ ""
        )
      )

    minefield_plot <- ggplot(
      mf_df, aes(x = x, y = y, fill = as.factor(is_probed))
    ) +
      geom_tile(
        color = "white",
        lwd = 1,
        linetype = 1
      ) +
      geom_text(aes(label = display_label), color = "black", size = 3) +
      coord_fixed() +
      theme(legend.position = "none")

    for (idx in seq_len(nrow(mf_df))) {
      tile_row <- mf_df[idx, ]
      if (tile_row$is_flagged) {
        sprite <- assets$TileFlag
      } else if (tile_row$is_probed &&
        tile_row$mines_near > 0 &&
        !tile_row$is_mine) {
        sprite <- assets[[paste0("Tile", mf_df$mines_near[idx])]]
      } else if (tile_row$is_probed && !tile_row$is_mine) {
        sprite <- assets$TileEmpty
      } else {
        sprite <- assets$TileUnknown
      }

      tile_x <- tile_row$x
      tile_y <- tile_row$y

      minefield_plot <- minefield_plot +
        annotation_custom(
          sprite,
          xmin = tile_x - .5,
          xmax = tile_x + .5,
          ymin = tile_y - .5,
          ymax = tile_y + .5
        )
    }

    minefield_plot
  })

  output$info <- renderText({
    #' Show number of mines in game and the number of flagged tiles in a message
    #' box on screen.
    mf <- minefield()
    mf_df <- mf$as_df()
    paste0(
      "Mines in game: ", sum(mf_df$is_mine), "\n",
      "Flags placed: ", sum(mf_df$is_flagged)
    )
  })

  observeEvent(input$field_click, {
    #' Probe a tile if the user double clicks it.
    #' Restart the game if a mine is probed and print a message to console.
    mf <- minefield()

    x_click <- round(input$field_click$x)
    y_click <- round(input$field_click$y)

    probe_res <- mf$probe_tile(x_click, y_click)

    if (probe_res == -1) {
      print("Oops that was a mine :( Restarting game")
      minefield(Field$new(width = cfg$field_width, height = cfg$field_height))
    } else {
      minefield(mf$clone())
    }
  })

  observeEvent(input$field_dblclick, {
    #' Flag a tile if the user double clicks it
    mf <- minefield()

    x_click <- round(input$field_dblclick$x)
    y_click <- round(input$field_dblclick$y)

    mf$flag_tile(x_click, y_click)

    minefield(mf$clone())
  })
}

shinyApp(ui, server)
