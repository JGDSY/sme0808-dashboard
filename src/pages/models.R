library(shiny)
library(bslib)


hypothesis_div <- div(

)

residuals_div <- div(
    div(
        class='residuals',
        textOutput('residual_text'),
        shinycssloaders::withSpinner(plotlyOutput('residuals'))
    )
)

prediction_div <- div(
    div(
        class='forecast',
        shinycssloaders::withSpinner(plotOutput('forecast'))
    )
)

adjust_metric_div <- div(
    tableOutput('table_metrics')
)

models_page <- div(

    div(
        class="model_selection",
        radioButtons("model_choice", "Escolha de Modelo",
                c(
                    "Auto ARIMA (Seleção Automatica)" ,
                    "ARIMA" ,
                    "AR",
                    "MA"
                ),
                selected="Auto ARIMA (Seleção Automatica)"
        ),
        conditionalPanel(
            "input.model_choice=='ARIMA' || input.model_choice=='AR' || input.model_choice=='ARMA'",
            numericInput("ARIMA_p",
                "Parametro P (AR)", 2, min = 1, max = 100
            ),
        ),
         conditionalPanel(
            "input.model_choice=='ARIMA'",
            numericInput("ARIMA_i",
                "Parametro i (Diff)", 2, min = 1, max = 100
            ),
         ),
          conditionalPanel(
            "input.model_choice=='ARIMA' || input.model_choice=='MA' || input.model_choice=='ARMA'",
            numericInput("ARIMA_q",
                "Parametro Q (MA)", 2, min = 1, max = 100
            )
        ),

        sliderInput("steps_slider", "Passos de Teste", min = 0, max = 30, value = 0),

        actionButton("model_selection", "Confirmar Modelo!"),
        actionButton("model_reset", "Resetar Modelo!")


    ),
    shinyjs::hidden(div(
        id="tab_models",
        tabsetPanel(
            type="tabs",
            tabPanel("Métricas de Ajuste", adjust_metric_div),
            tabPanel("Previsão", prediction_div),
            tabPanel("Analise de Residuos", residuals_div),
            tabPanel("Testes de Hipotese", hypothesis_div),
        )

    ))

)

