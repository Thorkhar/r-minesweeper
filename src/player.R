library(shiny)
library(ggplot2)
library(R6)
library(dplyr)

source("src/class/Field.R")
source("src/lib.R")

ui <- basicPage(
  plotOutput("field", click = "field_click", dblclick = "field_dblclick")
)

server <- function(input, output) {
  minefield <- reactiveVal(
    Field$new(width = 10, height = 10)
  )

  output$field <- renderPlot({
    mf <- minefield()
    mf_df <- convert_field_to_df(mf$tiles)

    ggplot(mf_df, aes(x = x, y = y, fill = as.factor(is_probed))) +
      geom_tile(
        color = "white",
        lwd = 1,
        linetype = 1
      ) +
      geom_text(aes(label = display_label), color = "black", size = 3) +
      coord_fixed()
  })

  observeEvent(input$field_click, {
    mf <- minefield()

    x_click <- round(input$field_click$x)
    y_click <- round(input$field_click$y)

    mf$probe_tile(x_click, y_click)

    minefield(mf$clone())
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
