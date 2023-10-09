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
        div(
            class="custom_filters",
            div(
                frequency_type_filter
            ),
            div(
                frequency_average_type,
                conditionalPanel(
                    condition = "input.frequency_average_type_input == 'Média Movel'",
                    frequency_moving_average_window
                ),
                conditionalPanel(
                    condition = "input.frequency_average_type_input == 'Diferenciação'",
                    frequency_differentiation_order
                )
            )
            
        ),
        div(
            class="plot_principal",
            align = "center",
            plotlyOutput("frequency_plot", width = "90%")
        )
    ),
)


seasonal_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise de Sazonalidade"
    ),
    div(
        class="custom_filters",
        seasonal_type_filter,
        div(
            seasonal_window_year_filter,
            seasonal_window_yearweek_filter,
        ),
        div(
            seasonal_window_weekday_filter,
            seasonal_window_months_filter
        )
    ),
    div(
        class="plot_principal",
        align = "center",
        plotlyOutput("seasonal_plot", width = "90%")
    )
    
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


transformation_plot_div <- div(
    class = "plot_box",
    div(
        class="plots_title",
        "Analise da Transformada de Box-Cox"
    ),
    div(
        class="plot_principal",
        align = "center",
        plotlyOutput("transformation_plot", width = "90%"),
        div(class='space'),
        plotlyOutput("transformation_plot2", width = "90%")
    ),
)