library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)

new_media_content <- "

.column0 {
  float: left;
  width: 2%;
}

.column1 {
  float: left;
  width: 28%;
  border-radius: 25px;
  background: #bfbfbf;
  padding: 20px;
  padding-left: 30px;
  margin-left: 30px;

  padding-right: 10px;
  margin-right: 10px;

  height: 90vh;

  box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.5);

}

.column2 {
  float: left;
  width: 66%;
  border-radius: 25px;
  background: #949494;
  padding: 20px;
  padding-left: 10px;
  margin-left: 10px;

  padding-right: 10px;
  margin-right: 10px;

  height: 90vh;

  box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.5);
}

.row:after {
  content: '';
  display: table;
  clear: both;
} 


html {background: #89a0ad}

#controls {
  padding: 0 20px 20px 20px;

}

.body_ui {
    background: #89a0ad;
}

.sidebar_topdiv {
    padding-top: 20px;
}

.sidebar_topdiv2 {
    padding-top: 10px;
}

.tools_group {
    border: 2px solid black;
    border-radius: 25px;
    padding-left: 30px;
}

.tools_group_simple {
    border-radius: 25px;
    padding-left: 30px;
}

.text_toolbar {
    padding-top: 10px;
    font-size: 18px;
}

.tools_group_top {
    text-align: center;
}

.columns_map_timeseries:after {
  content: '';
  display: table;
  clear: both;
} 

.timeseries {
    float: left;
    width: 70%;
    border-radius: 25px;
}

.map {
    float: left;
    width: 30%;
    border-radius: 25px;
}

"



coluna1 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    div(
        class='sidebar_topdiv2'
    ),
    div(
        class="tools_group_top",
         h3("Filtros"),

    ),
   
    div(
        class='sidebar_topdiv'
    ),
    div(
        class="tools_group_simple",
        selectInput(
        'tipo',
        'Selecione o Tipo da Analise',
        c("Notificações de SRAG", "Vacinação (Covid-19)"),

    ),
    ),
    div(
        class='sidebar_topdiv2'
    ),
    div(
        class="tools_group_simple",
        div(
                class='sidebar_topdiv2'
            ),
            pickerInput(
            'estado',
            'Selecione o Estado',
            c("AM","ES","SP","RS","PR","PE","MA","CE","MG","RJ","DF","BA","TO","SC","RO",
"PA","MT","PI","GO","MS","PB","RN","SE","AL","RR","AP","AC"), multiple = TRUE,
            selected= c("SP"),
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
        class='sidebar_topdiv2'
    ),
    div(
        class="tools_group_simple",
        div(
            class='sidebar_topdiv2'
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
            "2019",  "2023"
        ),
    )
    )
)

coluna2 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    div(
        class='columns_map_timeseries',
        div(
            class='timeseries',
            plotlyOutput("timeseries_statistics", width = '90%')
        ),
        div(
            class='map',
           leafletOutput("map")
        )
    )
    
)

home_content <- div(
  tags$head(tags$style(HTML(new_media_content))),
  class = 'body_ui',
  div(
    class = "row",
    div(
        class = "column0",
    ),
    div(
        class = "column1",
        id = "controls",

        coluna1
    ),
    div(
        class = "column2",
        coluna2
    )
  )
)