library(shiny.router)
library(shiny)

frequency_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise da Serie Temporal"
    ),
     div(
        class="plot_principal",
        align = "center",
        plotlyOutput("frequency_plot", width = "90%")
    ),
)


seasonal_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise de Sazonalidade"
    ),
    div(
        class="plot_principal",
        align = "center",
        plotlyOutput("seasonal_plot", width = "90%")
    ),
    
)

subseries_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise das Subseries"
    ),
    div(
        class="plot_principal",
        align = "center",
        plotlyOutput("subseries_plot", width = "90%")
    ),
)

lag_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise da Defasagem"
    ),
    div(
        class="plot_principal",
        align = "center",
        plotlyOutput("lag_plot", width = "90%")
    ),
)