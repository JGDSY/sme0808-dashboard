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
.tab-pane {
  padding-top: 90px;
}

.panel_title, .analysis_title{
  padding-left: 70px;
}

#filters-nav {
  position: fixed;
  top: 50px;
  color: #bababa;
  background-color: #777;
  padding: 0 30px 0 0;
  transition: 0.4s;
  width: 100%;
  height: 30px;
  display: flex;
  flex-flow: row nowrap;
  justify-content: space-between;
}

#filters-nav > * {
  margin: 5px 0;
}

#show_filters {
  background: inherit;
  color: white;
  border-color: white;
  padding: 0 25px;
  margin: 1px;
  border-radius: 1px;
}

.filters-box{
  z-index: 1000;
  position: fixed;
  top:7%;
  display: flex;
  flex-flow: row wrap;
  justify-content: space-around;
  max-width: 80%;
  left: 10%;
  background: white;
  border-radius: 10px;
  box-shadow: 0 0 10000px 10000px rgba(0,0,0,0.4), 0 0 0.3in 0.2in rgba(0, 0, 0, 0.2);
  padding: 30px 15px;
}

.filters-box > *{
  margin: 0 5px;
}

.filters-box > .filters-footer{
  width: 100%;
  height: auto;
  display: flex;
  justify-content: right;
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

.navbar-default, .navbar-inverse {
    background-color: #545454 !important;
    font-size: 100%;
    font-family: Arial;
    color: black;
    position: fixed;
    width: 100%;
    transition: 0.4s;
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
"

js_script <- "
window.addEventListener('load', function (event){
  screen_sizing(event);
  hide_modal(event);
  $('#confirm_filters').on('click', hide_modal);
  $('#show_filters').on('click', show_modal);
}, true);

window.addEventListener('resize', screen_sizing, true);

function screen_sizing(event) {
  let nav = $('nav.navbar');
  $('#filters-nav').css('top', (nav.position().top + nav.height()) + 'px');
  $('.tab-pane').css('padding-top', (nav.height() + $('#filters-nav').height()) + 'px');
}

async function show_modal(event){
  $('.filters-box').show('fast');
  $('#show_filters').off('click')
  await new Promise(r => setTimeout(r, 100));
  $(document).on('click', hide_modal_easy);
}

function hide_modal(event){
  $('.filters-box').hide('fast');
  $(document).off('click', hide_modal_easy);
  $('#show_filters').on('click', show_modal);
  
  
  let input_ids = ['analysis_type_input', 'year_filter_input', 'age_window_filter_input', 'ethnicity_type_input', 'state_filter_input', 'sickness_filter_input', 'ethnicity_type_filter_input1'];
  $('#filters-nav > span').each( function(index){
    let elem = $('#' + input_ids[index]);
    let content = elem.val();
    if(index >= 3 && index <= 5) { content = elem.val().length}
    $(this).text(content);
  })
}

function hide_modal_easy(event){
  let $target = $(event.target);
  if(!$target.closest('.filters-box').length && 
  $('.filters-box').is(':visible')) {
    $('#confirm_filters').trigger('click');
  }
}

window.addEventListener('scroll', function (event) {
  let nav = $('nav.navbar');
  if (document.body.scrollTop > (nav.height() - 5) || document.documentElement.scrollTop > (nav.height() - 5)) {
    nav.css('top', '-' + nav.height() + 'px');
    $('#filters-nav').css('top', '0px')
  } else {
    nav.css('top', '0');
    $('#filters-nav').css('top', (nav.height()) + 'px')
  }
}, true)
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
    id = "tabs",
    inverse = TRUE,
    title = "SME0808 - DATASUS",
    tabPanel(title = "Exploração visual", descriptive_analysis_page),
    tabPanel(title = "Tratamento de Tendencia e Sazonalidade", modeling_page),
    tabPanel(title = "Modelos preditivos", models_page),
    tabPanel(title = "Integrantes", about_page),
  ),
  div(
    id="filters-nav",
    actionButton("show_filters", "Filtros", class="filters-btn"),
    span(),
    span(),
    span(),
    span(),
    span(),
    span(),
    span(),
  ),
  div(
    class="filters-box",
    filters1,
    filters2,
    filters3,
    map_filter,
    div(
      class="filters-footer",
      actionButton("confirm_filters", "OK", class="close-btn")      
    )
  )
)
