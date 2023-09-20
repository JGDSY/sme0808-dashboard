library(shiny.router)
library(shiny)

source("./src/pages/home.R")

router <- make_router(
  route("/", home_page)
)