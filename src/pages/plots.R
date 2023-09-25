library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)

new_media_content <- "
.body_ui {
  width: inherit;
  display: grid;
  grid-template-columns: 1fr 3fr;
  grid-column-gap: 2%;
  padding: 0 10px
}

.body_ui > * {
  color: #333;
}

@media (min-width: 1150px) {
  .column_controls {
    position: fixed;
    margin-left: 1%;
  }
  
}

@media (max-width: 1149px) {
  .body_ui > .column_empty {
    display: none;
  }
}

.column_controls {
  height: 80vh;
  z-index: 100;
  max-width: 300px;
  min-width: 20%;
  
  background: #bfbfbf;
  border-radius: 8px;
  padding: 20px;
  
  display: flex;
  flex-flow: column nowrap;
  justify-content: space-around
}

.column_plots {
  background: white;
  border-radius: 8px;
  padding: 20px;

  display: flex;
  flex-flow: row wrap;
  justify-content: space-between
}

.timeseries {
  width: 70%;
  min-width: 550px;
}

.map {
  width: 30%;
  min-width: 220px;
}
"



coluna1 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    class = "column_controls",
    div(
        class = "tools_group_top",
         h3("Filtros"),

    ),
    div(
        class = "tools_group_simple",
        selectInput(
        "tipo",
        "Selecione o Tipo da Analise",
        c("Notificações de SRAG", "Vacinação (Covid-19)"),

    ),
    ),
    div(
        class = "tools_group_simple",
        div(
                class = "sidebar_topdiv2"
            ),
            pickerInput(
            "estado",
            "Selecione o Estado",
            c("AM", "ES", "SP", "RS", "PR", "PE", "MA", "CE", "MG", "RJ", "DF", "BA", "TO", "SC", "RO",
"PA", "MT", "PI", "GO", "MS", "PB", "RN", "SE", "AL", "RR", "AP", "AC"), multiple = TRUE,
            selected = c("SP"),
            options = pickerOptions(
            actionsBox = TRUE,
            size = 10,
            selectedTextFormat = "count > 3"
        ),

        ),
        radioButtons("estado_tipo", "Tipo de Comparação de Estados",
                c("Conjunta", "Individual")
        ),
    ),
    div(
        class = "tools_group_simple",
        div(
            class = "sidebar_topdiv2"
        ),
        sliderTextInput(
        inputId = "yearinterval",
        label = "Intervalo de Ano:",
        grid = TRUE,
        force_edges = TRUE,
        choices = c(
            "2019", "2020",
            "2021", "2022", "2023"
        ),
        selected = c(
            "2019", "2023"
        ),
    )
    )
)

coluna2 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    class = "column_plots",
    div(
        class = "timeseries",
        plotlyOutput("timeseries_statistics", width = "90%")
    ),
    div(
        class = "map",
       leafletOutput("map")
    )
)

plots_page <- div(
  tags$head(tags$style(HTML(new_media_content))),
  class = "body_ui",
  div(
    class = "column_empty"
  ),
  coluna1,
  coluna2
)