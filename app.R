library(shiny)
library(leaflet)
library(plotly)
library(rgdal)


source("./src/components/route.R")

ui <- bootstrapPage(div(router$ui))

df <- read.csv("./data/2020_SRAG.csv")
df$DT_NOTIFIC <- as.Date(df$DT_NOTIFIC, "%d/%m/%Y")
df$value <- 1

print(unique(df$SG_UF))

server <- function(input, output, session) {
    ns <- session$ns
    mapa  <- readOGR("./resources/", "BR_UF_2022", stringsAsFactors=FALSE, encoding="UTF-8")

    output$map <- renderLeaflet({
        geneva_map <- leaflet(mapa) %>%
            addTiles() %>%
            addPolygons(
                color = "#444444",
                weight = 1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.1
            )

        geneva_map
    })

    state = reactive({input$estado})
    print(state)
    df_aux = reactive({
        return(filter(df, SG_UF %in% state()))
    })

    df_ts = reactive({
        aggregate(df_aux()["value"], by=df_aux()["DT_NOTIFIC"], sum)
    })



    output$timeseries_statistics <- renderPlotly({
        fig <- plot_ly(df_ts(), type = 'scatter', mode = 'lines')%>%
        add_trace(x = ~DT_NOTIFIC, y = ~value, name = 'Notificações')%>%
        layout(showlegend = F)



        fig

    })



    router$server(input, output, session)

    
}

shinyApp(ui, server)