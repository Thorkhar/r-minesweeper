library(shiny)
library(ggplot2)

ui <- basicPage(
  plotOutput("field", click = "field_click")
)

server <- function(input, output) {
  minefield <- reactiveVal(
    dummy <- expand.grid(x = 1:10, y = 1:10) |>
      dplyr::rowwise() |>
      dplyr::mutate(value = rbinom(1, 1, .5))
  )

  output$field <- renderPlot({
    mf <- minefield()
    ggplot(mf, aes(x = x, y = y, fill = as.factor(value))) +
      geom_tile(
        color = "white",
        lwd = 1,
        linetype = 1
      ) +
      coord_fixed()
  })

  observeEvent(input$field_click, {
    mf <- minefield()
    x_click <- round(input$field_click$x)
    y_click <- round(input$field_click$y)

    mf[mf$x == x_click & mf$y == y_click, ]$value <- 2

    minefield(mf)
  })
}

shinyApp(ui, server)
