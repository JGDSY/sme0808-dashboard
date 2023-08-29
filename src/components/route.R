library(shiny.router)
library(shiny)


source("./src/pages/about.R")
source("./src/pages/home.R")

router <- make_router(
  route("/", home_page),
  route("about", about_page)
)