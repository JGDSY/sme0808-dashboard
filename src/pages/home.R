library(shiny)
library(bslib)

source("./src/pages/about.R")
source("./src/pages/home_content.R")
source("./src/pages/table_page.R")

new_media <- "

html {background: #89a0ad}

.headerdiv {
  background: #89a0ad;
}


@media (max-width: 955px) {
    .navbar-header .navbar-brand {float: left; text-align: center; width: 100%}
    .navbar-header { width:100% }
.navbar-brand { width: 100%; text-align: center }

}

.container-fluid {
    padding-right: 0px;
    padding-left: 0px;
    margin-right: auto;
    margin-left: auto;
}

.nav
{
    padding-left: 15px;

}

.navbar-inverse .navbar-brand{
  color: white
}

.navbar-brand:hover {
    color: white;
}


@media (min-width: 956px) {
    .navbar {width:100%}
    .navbar .navbar-nav {float: right}
    .navbar .navbar-header {float: left}
    .navbar-brand { float: left; padding-left: 30px;  }
}

.content-home{
width: 100%
}

.map-div{
  width: 60%;
  margin-left: 30px
}

"

ns <- NS('home')
dimensionId <- ns("dimension")

home_page <- fluidPage(
  tags$head(
    tags$style(HTML(new_media)),
    tags$script('
    var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
  ', dimensionId)
  ),
  class="headerdiv",
  
  navbarPage(
    inverse = T,
    title = "SME0808",
    tabPanel(title = "Analise da Serie Temporal", home_content),
    tabPanel(title = "Tabela", table_page),
    tabPanel(title = "Sobre", about_page),
    tags$style(HTML(new_media))
  ),
  
)