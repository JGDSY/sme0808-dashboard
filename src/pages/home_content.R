library(shiny)
library(bslib)

new_media_content <- "

.column0 {
  float: left;
  width: 2%;
}

.column1 {
  float: left;
  width: 28%;
  border-radius: 25px;
  background: #666666;
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
  background: #666666;
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

#   opacity: 0.65;
}

.body_ui {
    background: #89a0ad;
}
"

coluna1 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    "Coluna 1",
    selectInput(
        'tipo',
        'Selecione',
        c("Notificações de SRAG", "Vacinação"),

    ),
    selectInput(
        'estado',
        'Selecione o Estado',
        c("Todos", "SP", "MG", "RJ"),

    )
)

coluna2 <- div(
    tags$head(tags$style(HTML(new_media_content))),
    "Coluna 2"
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