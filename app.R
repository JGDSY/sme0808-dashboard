library(shiny)
library(leaflet)
library(plotly)


source("./src/components/route.R")

ui <- bootstrapPage(div(router$ui))

server <- function(input, output, session) {
    router$server(input, output, session)
}

shinyApp(ui, server)