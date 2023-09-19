library(shiny)
library(bslib)

source("./src/pages/about.R")
source("./src/pages/plots.R")
source("./src/pages/models.R")

new_media <- "

html {
  background: #89a0ad;
  width: 100vw;
  overflow-x: hidden;
}

body{
  background: #89a0ad
}

*:before, *:after{
  content: none !important
}

.headerdiv {
  background: #89a0ad;
}

.navbar > .container-fluid{
  padding: 0 15px;
  display: flex;
  flex-flow: row wrap;
  justify-content: space-between;
}

.container-fluid{
  padding: 0px;
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

"

ns <- NS('home')
dimensionId <- ns("dimension")

home_page <- fluidPage(
  tags$head(
    tags$style(HTML(new_media))
  ),
  class="headerdiv",
  
  navbarPage(
    inverse = T,
    title = "SME0808",
    tabPanel(title = "Exploração visual", plots_page),
    tabPanel(title = "Modelos preditivos", models_page),
    tabPanel(title = "Integrantes", about_page)
  ),
  
)