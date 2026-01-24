library(shiny)
library(ggplot2)
library(R6)
library(dplyr)

options(shiny.port = 8100, shiny.host = "0.0.0.0")
cfg <- jsonlite::fromJSON("settings.json")
source("src/class/Field.R")

ui <- basicPage(
  plotOutput("field", click = "field_click", dblclick = "field_dblclick"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  minefield <- reactiveVal(
    Field$new(width = cfg$field_width, height = cfg$field_height)
  )

  output$field <- renderPlot({
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

    ggplot(mf_df, aes(x = x, y = y, fill = as.factor(is_probed))) +
      geom_tile(
        color = "white",
        lwd = 1,
        linetype = 1
      ) +
      geom_text(aes(label = display_label), color = "black", size = 3) +
      coord_fixed() +
      theme(legend.position = "none")
  })

  output$info <- renderText({
    mf <- minefield()
    mf_df <- mf$as_df()
    paste0(
      "Mines in game: ", sum(mf_df$is_mine), "\n",
      "Flags placed: ", sum(mf_df$is_flagged)
    )
  })

  observeEvent(input$field_click, {
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
    mf <- minefield()

    x_click <- round(input$field_dblclick$x)
    y_click <- round(input$field_dblclick$y)

    mf$flag_tile(x_click, y_click)

    minefield(mf$clone())
  })
}

shinyApp(ui, server)
