library(shiny)
library(bslib)

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

        actionButton("model_selection", "Confirmar Modelo!")


    ),
    div(
        class=""
    ),
    div(
        class='metrics',
        tableOutput('table_metrics')
    ),
    div(
        class='residuals',
        textOutput('residual_text'),
        plotlyOutput('residuals')
    ),
    div(
        class='forecast',
        plotOutput('forecast')
    )
    # plotlyOutput()

)