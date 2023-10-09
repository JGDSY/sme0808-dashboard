library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
source("./src/components/card_data.R")
source("./src/components/filters.R")
source("./src/components/descriptive_plots.R")


css <- "
.cards_panel {
  background-color: #FFFFFF;
  padding-left: 15px;
  padding-right: 10px;
  padding-top: 15px;
  height: 190px;
  border-radius: 5%;
  border-color: rgba(170, 190, 210, .7);
  border-style: solid;
  max-width: 250px;
  min-width: 150px;
}

.card_columns {
  display: grid;
  grid-column-gap: 20px;
  grid-template-columns: repeat(4, 1fr);
  padding-left: 40px;
  padding-right: 40px;
}

.panel_title {
    padding-left: 40px;
    padding-bottom: 30px;
    padding-top: 40px;
    font-size: 22px;
    font-family: 'Montserrat';
    font-weight: 600;
}

.analysis_title {
    padding-left: 40px;
    padding-bottom: 30px;
    padding-top: 60px;
    font-size: 22px;
    font-family: 'Montserrat';
    font-weight: 600;
}

.cards_panel_main_text {
    font-size: 13px;
    font-family: 'Montserrat';
    font-weight: 700;
    padding-bottom: 13px;
}

.cards_panel_main_values {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
}

.cards_panel_value_name {
    font-size: 9px;
    font-family: 'Montserrat';
    font-weight: 200;
}

.cards_panel_value_value {
    font-size: 13px;
    font-family: 'Montserrat';
    font-weight: 700;
}

.cards_panel_graph {
    padding-top: 30px;
}

.analysis_filters {
    display: grid;
    grid-column-gap: 20px;
    grid-template-columns: repeat(4, 1fr);
    padding-left: 40px;
    padding-right: 40px;
}

.filter_text {
    font-size: 15px;
    font-family: 'Montserrat';
    font-weight: 500;
}

.filter_selection {
    .bootstrap-select.show-tick .dropdown-menu .selected span.check-mark {left: 1px; right: auto !important}
}

.filter_set {
    padding-bottom: 40px;
}

.plots_title {
    padding-left: 40px;
    padding-bottom: 30px;
    padding-top: 40px;
    font-size: 18px;
    font-family: 'Montserrat';
    font-weight: 500;
}

.plot_box {
    width: 100%;
    align: center;
    padding-top: 30px;
    padding-bottom: 40px;
}

.main-svg {
    border-radius: 2%;
    box-shadow: rgba(100, 100, 111, 0.3) 0px 5px 23px 0px;
}

.space {
    padding-top: 30px;
}

.custom_filters {
    display: grid;
    padding-left: 40px;
    padding-right: 40px;
    grid-column-gap: 20px;
    grid-template-columns: repeat(4, 1fr);
}

"

descriptive_analysis_page <- div(
    tags$head(
        tags$style(HTML(css)),
        tags$link()
    ),
    div(
        class="panel_title",
        "Painel da Sindrome Respiratoria Aguda Grave (SRAG)"
    ),
    div(
        class="card_columns",
        card_data("Notificaçoes de SRAG", textOutput("acc_srag"), "Acumulado", textOutput("mean_srag"), "Media por Dia", "mini_srag_plot"),
        card_data("Hospitalização", textOutput("acc_internacao"), "Acumulado", textOutput("mean_internacao"), "Media por Dia", "mini_internacao_plot"),
        card_data("Internações em UTI", textOutput("acc_uti"), "Acumulado", textOutput("mean_uti"), "Media por Dia", "mini_uti_plot"),
        card_data("Notificaçoes de Obito", textOutput("acc_obito"), "Acumulado", textOutput("mean_obito"), "Media por Dia", "mini_obito_plot"),

    ),
    div(
        class="analysis_title",
        "Analise Descritiva e Exploratoria"
    ),
    div(
        class="analysis_filters",
        filters1,
        filters2,
        filters3,
        map_filter
    ),
    div(
        class="tabs_plot",
        tabsetPanel(
            type="tabs",
            tabPanel("Serie Temporal", frequency_plot_div),
            tabPanel("Sazonalidade", seasonal_plot_div),
            tabPanel("Subseries", subseries_plot_div),
            tabPanel("Defasagem", lag_plot_div),
            tabPanel("Transformação de Box-Cox", transformation_plot_div)
        )
    ),

)