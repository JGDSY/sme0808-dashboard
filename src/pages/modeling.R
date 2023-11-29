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
    font-size: 18px;
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
    width: 500px;

    font-weight: 500;
    padding-left: 40px;
    padding-bottom: 10px;
    padding-top: 40px;
    font-size: 22px;
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
    font-size: 18px;
    font-family: 'Montserrat';
    font-weight: 500;
}

.transformation_plot {
    padding-top: 30px;
    padding-bottom: 30px;
    padding-right: 50px;
}

.transformation_confirm {
    padding-bottom: 30px;
}

"





transformation_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        '',
        bsPopover(
            "transformation_variance", "Etapa de Estacionariedade",
            "A primeira etapa na modelagem de uma serie temporal está relacionada com remover a variancia dos dados, garantindo a estacionariedade da serie. <br> <br> Para isso, existem alguns métodos: <br> <ul> <li> Box-Cox: Se lambda for diferente de zero, ocorre a aplicação da operação (x^(lambda) -1)/(lambda * GM(x)^(lambda-1)) para todos os dados da serie, caso contrario, GM(x)*ln(x)  </li> <li> Log: Aplicação da operação ln(x) para todos os dados da serie</li> <li> Raiz Quadrada: Aplicação da operação x^(1/2) para todos os dados da serie</li> </ul>",
        options = list(container = "body"))
        #  tooltip(
        #     bs_icon("info-circle"),
        #     "Tooltip message",
        #     placement = "bottom"
        # )
    ),
    div(
        class='transformation_item',
        
        radioButtons("transformation_variance", "1. Método de Transformação de Variancia",
            c(
                "Não Aplicar" ,
                "Box-Cox" ,
                "Log",
                "Raiz Quadrada"
            ),
            selected="Box-Cox"
        )
    ),
    div(
        class="transformation_plot",
        plotlyOutput("variance_plot", width = "100%", height="580px"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_transformation", "Confirmar Transformação"),
    )
)

tendency_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        '',
        bsPopover(
            "transformation_tendency", "Etapa de Retirada da Tendencia",
            "Na segunda etapa acontece a retirada da tendencia da serie temporal. <br> <br> Existem diversos métodos para realização dessa etapa: <br> <ul><li>Media Movel: </li>  <li>Diferenciação:</li></ul>",
        options = list(container = "body"))
    ),
    div(
        class='transformation_item',
        radioButtons("transformation_tendency", "2. Método de Transformação de Tendencia:",
            c(
                "Não Aplicar",
                "Media Movel", 
                "Diferenciação"
            )
        ),
        conditionalPanel(
            "input.transformation_tendency=='Media Movel'",
            numericInput("transformation_rollavg_input",
            "Janela da Media Movel", 1, min = 1, max = 1000)
        ),
        conditionalPanel(
            "input.transformation_tendency=='Diferenciação'",
            numericInput("transformation_diff_input",
            "Ordem de Diferenciação", 1, min = 1, max = 10)
        )

    ),

    div(
        class="transformation_plot",
        plotlyOutput("tendency_plot", width = "100%"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_tendency", "Confirmar Transformação"),
    )
)

autocorrelation_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        '4. Analise da Autocorrelação',
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

decomposition_modeling <- div(
    class='transformation_div',
    div(
        class='transformation_text',
        '',
        bsPopover(
            "transformation_decomposition", "Etapa de Decomposição da Serie",
            "A ultima etapa da modelagem consiste em extrair e visualizar em diferentes graficos a tendencia, a sazonalidade e o ruido.",
        options = list(container = "body"))
    ),
    div(
        class='transformation_item',
        radioButtons("transformation_decomposition", "3. Método de Decomposição da Serie",
            c(
                "Não Aplicar" ,
                "Decomposição Aditiva",
                "Decomposição Multiplicativa" 
            ),
        )
    ),
    div(
        class="transformation_plot",
        plotlyOutput("decomposition_plot", width = "100%", height="640px"),
    ),
    div(
        class="transformation_confirm",
        actionButton("confirm_decomposition", "Confirmar Transformação"),
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
#     div(
#        class="modeling_filters",
#        filters1,
#        filters2,
#        filters3,
#    ),


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
                tendency_modeling,
                conditionalPanel(
                    condition="input.confirm_tendency==1",
                    div(
                        decomposition_modeling,
                        conditionalPanel(
                            condition="input.confirm_decomposition==1",
                            autocorrelation_modeling
                        )
                    )
                )
            )
        )
    )
)