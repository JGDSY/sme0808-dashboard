library(shiny.router)
library(shiny)

card_value <- function(valor, texto){
    card_value_div <- div(
        class="cards_panel_value",
        div(
            class="cards_panel_value_value",
            valor
        ),
        div(
            class="cards_panel_value_name",
            texto
        )
    )

    return(card_value_div)
}

card_data <- function(titulo, valor1, texto1, valor2, texto2, plot_key){
    card_div <- div(
        class="cards_panel",
        div(
            class="cards_panel_main_text",
            titulo
        ),
        div(
            class="cards_panel_main_values",
            card_value(valor1, texto1),
            card_value(valor2, texto2),
        ),
        div(
            class="cards_panel_graph",
            shinycssloaders::withSpinner(plotOutput(plot_key, height="60px"))
        )
    )

    return(card_div)
}