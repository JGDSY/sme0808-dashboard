library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)

css <- "

.modeling_column {
    margin-top:auto;
    margin-bottom:auto;
    # text-align:center;
    padding-left: 50px;
    # content-align: center;
    # justify-content: center;


}


#transformation_variance {
    width: 500px;
    font-size: 18px;
    font-family: 'Montserrat';
    font-weight: 500;
}

#transformation_tendency {
    width: 500px;
    font-size: 14px;
    font-family: 'Montserrat';
    font-weight: 500;
}

#transformation_tendency2 {
    width: 500px;
    font-size: 14px;
    font-family: 'Montserrat';
    font-weight: 500;
}

#transformation_decomposition {
    width: 500px;
    font-size: 18px;
    font-family: 'Montserrat';
    font-weight: 500;
}

.text_filter {
    width: 600px;

    font-weight: 500;
    padding-left: 40px;
    padding-bottom: 10px;
    padding-top: 40px;
    font-size: 30px;
    font-family: 'Montserrat';
}

.text_explanation_filter {
    width: 80%;

    font-weight: 400;
    padding-left: 40px;
    padding-bottom: 30px;
    padding-right: 100px;
    padding-top: 10px;
    font-size: 14px;
    font-family: 'Montserrat';
}

.transformation_div {
    padding-bottom: 30px;
}

.transformation_text {
    font-size: 26px;
    font-family: 'Montserrat';
    font-weight: 500;
    padding-bottom: 30px;
}

.transformation_plot {
    padding-top: 30px;
    padding-bottom: 30px;
    padding-right: 50px;
}

.transformation_confirm {
    padding-bottom: 30px;
}

.transformation_item{
    display: flex;
  flex-flow: row wrap;
  max-width: inherit;
}

"





transformation_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        'Modelagem da Tendencia',
        bsPopover(
            "transformation_tendency", "",
            "Texto Texto Texto",
        options = list(container = "body"))
    ),
    div(
        class='transformation_item',
            
        div(
            radioButtons("transformation_tendency2", "Função de Modelagem da Tendencia",
                c(
                    "Não Aplicar" ,
                    "Polinomio" ,
                    "Linear por Partes"
                ),
                selected="Não Aplicar"
            )
        ),
        div(
                radioButtons("transformation_tendency", "Transformação Para Tendencia",
                c(
                    "Não Aplicar",
                    "Polinomio",
                    "Log",
                    "Raiz Quadrada"
                ),
                selected="Não Aplicar"
            )
        ),
        div(
            conditionalPanel(
                "input.transformation_tendency=='Polinomio' || input.transformation_tendency2=='Polinomio'",
                numericInput("tendency_degree_input",
                "Grau do Polinomio", 2, min = 1, max = 100)
            )
        )
        
        
        
    ),
    div(
        class="transformation_plot",
        plotlyOutput("tendency_plot_1", width = "100%", height="580px"),
    ),
     div(
        class="transformation_plot",
        plotlyOutput("tendency_plot_2", width = "100%", height="580px"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_transformation", "Confirmar Transformação"),
    )
)

sazonality_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        'Modelagem da Sazonalidade',
        bsPopover(
            "transformation_sazonalidade", "Transformação para Sazonalidade",
            "Na segunda etapa acontece a retirada da tendencia da serie temporal. <br> <br> Existem diversos métodos para realização dessa etapa: <br> <ul><li>Media Movel: </li>  <li>Diferenciação:</li></ul>",
        options = list(container = "body"))
    ),
    div(
        class='transformation_item',
        radioButtons("transformation_sazonalidade", "Transformação Para Sazonalidade",
            c(
                "Senoide", 
                "Fourier"
            )
        ),
        div(
            
            conditionalPanel(
                "input.transformation_sazonalidade=='Senoide'",
                radioButtons("transformation_random", "Escolha dos Pametros da Senoid",
                c(
                    "Heuristica Padrão", 
                    "Definir Manualmente"
                )
                )
            ),

            
        ),
        conditionalPanel(
                "input.transformation_random=='Definir Manualmente'",
            
            conditionalPanel(
                "input.transformation_sazonalidade=='Senoide'",
                numericInput("transformation_sazonalitya",
                "Valor Inicial A", 1)
            ),
            conditionalPanel(
                "input.transformation_sazonalidade=='Senoide'",
                numericInput("transformation_sazonalityb",
                "Valor Inicial B", 2)
            ),
            conditionalPanel(
                "input.transformation_sazonalidade=='Senoide'",
                numericInput("transformation_sazonalityc",
                "Valor Inicial C",1)
            ),
            conditionalPanel(
                "input.transformation_sazonalidade=='Senoide'",
                numericInput("transformation_sazonalityd",
                "Valor Inicial D",1)
            )
            ),
        conditionalPanel(
            "input.transformation_sazonalidade=='Fourier'",
            numericInput("transformation_sazonalityfourrier",
            "Numero Maximo de Termos", 5, min = 1)
        )

    ),

    div(
        class="transformation_plot",
        plotlyOutput("sazonality_plot_1", width = "100%"),
    ),
     div(
        class="transformation_plot",
        plotlyOutput("sazonality_plot_2", width = "100%"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_sazonality", "Confirmar Transformação"),
    )
)

autocorrelation_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        'Analise da Autocorrelação',
    ),
    div(
        class="transformation_plot",
        plotlyOutput("autocorrelation_plot", width = "100%", height="640px"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_autocorrelation", "Confirmar Transformação"),
    )
)




modeling_page <- div(
    tags$head(
        tags$style(HTML(css)),
        tags$link()
    ),
    div(
        class="panel_title",
        "Painel de Tratamento de Tendencia e Sazonalidade "
    ),
    
    
#     div(
#         class="text_filter",
#         "Filtros Pré Modelagem"
#     ),
#     div(
#         class="text_explanation_filter",
#         bsCollapse(
#             id = "collapseExample2",
#             open = "Panel 2",
#             bsCollapsePanel(
#                 "Explicação da Etapa",
#                 HTML("Nessa seção você poderia filtrar quais dados serão usados para modelagem, ajuste e validação da serie temporal."),
#                 HTML("<br><br>Ou seja, se você desejar modelar uma serie temporal considerando apenas os dados do estado de São Paulo, basta filtrar o estado."),
#                 HTML(" Cabe destacar que os filtros aplicados nessa etapa serão levados para a etapa de modelagem preditiva e verificação de ajuste dados, portanto, escolha-os bem!"),
#                 style = "info"
#             )
#         )
# 
#     ),
# 



    div(
        class="text_filter",
        "Modelagem da Serie Temporal"
    ),
    div(
        class="text_explanation_filter",
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        # div(
        #     class="",
        #     "",
        # ),
        bsCollapse(
            id = "collapseExample",
            open = "Panel 2",
            bsCollapsePanel(
                "Explicação da Etapa",
                HTML("Nesta etapa de pré-processamento de séries temporais, você terá a oportunidade de aprimorar seus dados de séries temporais antes de prosseguir com a modelagem. Mesmo que você não tenha experiência em estatística, não se preocupe, pois todas as operações são projetadas para serem simples e acessíveis. <br> <br> As etapas de pré-processamento são dividas em: "),
                HTML("<br><br><p><b>1) Remoção de Variância</b></p>"),
                HTML("<br>Esta etapa permite que você controle a variabilidade dos seus dados. Variabilidade refere-se à amplitude das flutuações nos valores da série temporal. Às vezes, dados com alta variabilidade tornam difícil identificar tendências significativas ou padrões. A remoção de variância ajuda a suavizar essas flutuações, tornando os dados mais estáveis e fáceis de analisar."),
                HTML("<br><br><p><b>2) Remoção de Tendência</b></p>"),
                HTML("<br>Aqui, você poderá eliminar o componente de tendência presente nos seus dados. A tendência é uma representação da direção geral dos dados ao longo do tempo, como uma linha de tendência que mostra se os valores estão aumentando ou diminuindo. Ao remover a tendência, você estará 'nivelando' seus dados, o que facilita a identificação de flutuações de curto prazo. Em outras palavras, você estará destacando as variações que não estão relacionadas a uma tendência geral."),
                HTML("<br><br> <p><b>3) Verificação da Estacionariedade</b></p>"),
                HTML("<br> Nesta etapa, você poderá avaliar se sua série temporal é estacionária. Uma série estacionária é aquela em que as características estatísticas, como média e variância, permanecem constantes ao longo do tempo. Isso é fundamental para tornar a série temporal mais previsível e interpretável."),
                style = "info"
            )
        )

    ),
    div(
        class="modeling_column",
        transformation_modeling,
        
        conditionalPanel(
            condition="input.confirm_transformation==1",
            div(
                sazonality_modeling,
                conditionalPanel(
                    condition="input.confirm_sazonality==1",
                    div(
                        autocorrelation_modeling,

                    )
                )
            )
        )
    )
)