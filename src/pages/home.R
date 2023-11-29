library(shiny)
library(bslib)

source("./src/pages/about.R")
source("./src/pages/descriptive_analysis.R")
source("./src/pages/models.R")
source("./src/pages/modeling.R")

new_media <- "

@import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap');

html {
  background: #EAEBEC;
  width: 100vw;
  overflow-x: hidden;
}

body{
  background: #EAEBEC
}

.headerdiv {
  background: #EAEBEC;
}

.nav:before,
.nav:after,
.navbar:before,
.navbar:after,
.navbar-header:before,
.navbar-header:after,
.navbar-collapse:before,
.navbar-collapse:after,
.container:before,
.container:after,
.container-fluid:before,
.container-fluid:after {
  all: unset;
}

.navbar > .container-fluid{
  padding: 0 15px;
  display: flex;
  # flex-flow: row wrap;
  justify-content: space-between;
}

.container-fluid{
  padding: 0px;
}

modal-body{
  background-color: #EAEBEC !important;
}

.filters-btn {
  position: absolute;
  top: 50px;
  color: #bababa;
  background-color: #545454;
  padding: 10px 30px;
}

.filters-box{
  display: flex;
  flex-flow: row wrap;
  justify-content: space-around;
  max-width: inherit;
}

 /* width */
::-webkit-scrollbar {
  width: 10px;
}

/* Track */
::-webkit-scrollbar-track {
  background: #f1f1f1;
}

/* Handle */
::-webkit-scrollbar-thumb {
  background: #888;
}

/* Handle on hover */
::-webkit-scrollbar-thumb:hover {
  background: #555;
}

.navbar-default {
    background-color: #545454 !important;
    font-size: 100%;
    font-family: Arial;
    color: black;
}

.navbar-inverse {
    background-color: #545454 !important;
    font-size: 100%;
    font-family: Arial;
    color: #FFFFFF;
}

.navbar-inverse .navbar-brand {
  color: #FFFFFF;
}

.navbar-inverse .navbar-nav > li > a {
  color: #bababa;
}

.navbar-inverse .navbar-nav > .active > a{
  background-color: #3e3e3e;
  color: #FFFFFF;

}

.navbar-inverse .navbar-nav > .active > a:focus {
  background-color: #3e3e3e;
  color: #FFFFFF;

}

.navbar-inverse .navbar-nav > .active > a:hover {
  background-color: #3e3e3e;
  color: #FFFFFF;

} 

#dummy-filters{
  display:none
}


"

js_script <- "

window.addEventListener('load', screen_sizing, true);
window.addEventListener('resize', screen_sizing, true);

function screen_sizing(event) {
  let nav_height = document.getElementsByClassName('navbar')[0].offsetHeight;
  let filter_btn = document.getElementsByClassName('filters-btn')[0];
  filter_btn.style.top = (nav_height - 1) + 'px';
}

"

ns <- NS('home')
dimensionId <- ns("dimension")

home_page <- fluidPage(
  tags$head(
    tags$style(HTML(new_media)),
    tags$script(HTML(js_script))
  ),
  class="headerdiv",
  navbarPage(
    inverse = TRUE,
    title = "SME0808 - DATASUS",
    tabPanel(title = "Exploração visual", descriptive_analysis_page),
    tabPanel(title = "Tratamento de Tendencia e Sazonalidade", modeling_page),
    tabPanel(title = "Modelos preditivos", models_page),
    tabPanel(title = "Integrantes", about_page),
  ),
#filtros invisíveis para dar trigger na atualização dos dados
  div(
    id="dummy-filters",
    filters1,
    filters2,
    filters3,
    map_filter
  ),
  actionButton("show_filters", "Dados",class="filters-btn")
)
