library(shiny)
library(leaflet)
library(plotly)
library(rgdal)
library(dplyr)
library(data.table)
library(stringr)
library(bslib)
library(fpp3)
library(patchwork)
library(tidyverse)
library(zoo)
library(forecast)
library(ggplot2)
library(bsicons)
library(tippy)
library(shinyBS)
library(detrendr)
library(ggpubr)
library(changepoint)
library(strucchange)


Sys.setlocale("LC_ALL", "pt_PT.UTF-8")


source("./src/components/route.R")
source("./src/server/seasonal_plot.R")
source("./src/server/cards_plot.R")
source("./src/server/frequency_plot.R")
source("./src/server/subseries_plot.R")
source("./src/server/transformation_plot.R")
source("./src/server/modeling_plot.R")
source("./src/server/lag_plot.R")
source("./src/server/map.R")


ui <- bootstrapPage(
    div(router$ui)
)

print("Starting data loading and processing")

df <- fread("./data/srag_2012_2019_2_processed.csv")
# mapa  <- readOGR("./resources/", "BR_UF_2022", stringsAsFactors=FALSE, encoding="UTF-8")

# df$value <- 1
# df$DT_NOTIFIC <- as.Date(df$DT_NOTIFIC, "%d/%m/%Y")
# df$ano = as.numeric(format(df$DT_NOTIFIC,'%Y'))
# df$NU_IDADE_N = as.numeric(df$NU_IDADE_N)
# df$mes = as.numeric(format(df$DT_NOTIFIC,'%m'))
# df$dia = as.numeric(format(df$DT_NOTIFIC,'%d'))
# df$dia_ano = as.numeric(df$DT_NOTIFIC - as.Date(paste0(df$ano -1,'-','12','-','31')))
# df$dia_semana = weekdays(df$DT_NOTIFIC,abbreviate = T)
# df$dia_semana_num = NA
# ordem_dia_semana = c( "seg","ter","qua","qui","sex", "sáb", "dom")
# df[, dia_semana_num := match(dia_semana, ordem_dia_semana)]
# df$semana_ano = as.numeric(ceiling(df$dia_ano/7))
# df$ano_mes <- format(as.Date(df$DT_NOTIFIC), "%Y-%m")
# df$week_ano <- format(as.Date(paste(df$ano, df$semana_ano, 1, sep="-"), "%Y-%U-%u"))
df$obito <- (df$EVOLUCAO == 2)

df$ano <- as.numeric(df$ano)


etinia_map <- c(
    "Branca" = 1, "Preta" = 2, "Amarela" = 3,
    "Parda" = 4, "Indigena" = 5, "Não Especificado" = 9
)
sintomas_map <- c(
    "Febre" = "FEBRE", "Tosse" = "TOSSE",
    "Dor de Garganta" = "GARGANTA",
    "Dispneia" = "DISPNEIA",
    "Desconforto Respiratorio" = "DESC_RESP",
    "Saturação Baixa" = "SATURACAO",
    "Outros" = "OUTRO_SIN"
)


gc()

print(head(df))


df_agg_dia <- df[, sum(value), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_obito <- df[, sum(obito), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_obito <- na.omit(df_agg_obito)
df_agg_internacao <- df[, sum(HOSPITAL), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_uti <- df[, sum(UTI), by = DT_NOTIFIC][order(DT_NOTIFIC)]


print(head(df_agg_dia))
print(head(df_agg_obito))

# write.csv(df, "srag_2012_2023_2_processed.csv")


print("Ending data loading and processing")


server <- function(input, output, session) {

    output$acc_srag <- renderText({
        return(sum(df_agg_dia$V1))
    })

    output$acc_uti <- renderText({
        return(sum(df_agg_uti$V1))
    })

    output$acc_internacao <- renderText({
        return(sum(df_agg_internacao$V1))
    })

    output$acc_obito <- renderText({
        return(sum(df_agg_obito$V1))
    })

    output$mean_srag <- renderText({
        return(round(sum(df_agg_dia$V1)/length(df_agg_dia$V1), 1))
    })

    output$mean_uti <- renderText({
        return(round(sum(df_agg_uti$V1)/length(df_agg_dia$V1), 1))
    })

    output$mean_internacao <- renderText({
        return(round(sum(df_agg_internacao$V1)/length(df_agg_dia$V1), 1))
    })

    output$mean_obito <- renderText({
        return(round(sum(df_agg_obito$V1)/length(df_agg_dia$V1), 1))
    })

    etinias_input <- reactive({
        return(input$ethnicity_type_input)
    })

    clicklist <- reactiveValues(ids = c(
        "SP", "RJ", "MG", "ES", "SC", "RS", "PR",
        "DF"
    ))
    
    df_filtered <- reactive({



        age_window <- c(input$age_window_filter_input[1]:input$age_window_filter_input[2])
        year_window <- c(input$year_filter_input[1]: input$year_filter_input[2])

        df_aux <- filter(df, ano %in% year_window)
        df_aux <- filter(df_aux, CS_RACA %in% input$ethnicity_type_input)
        df_aux <- filter(df_aux, NU_IDADE_N %in% age_window)
        df_aux <- filter(df_aux, SG_UF %in% input$state_filter_input)

        for (i in input$sickness_filter_input){
            col_name <- sintomas_map[i]
            df_aux <- filter(df_aux, get(col_name) %in% c(1))
        }

        if(input$analysis_type_input == "Obitos"){
            df_aux <- filter(df_aux, obito %in% c(TRUE))
        } else if(input$analysis_type_input == "Internação"){
             df_aux <- filter(df_aux, HOSPITAL %in% c(1))
        } else if(input$analysis_type_input == "Internação em UTI"){
             df_aux <- filter(df_aux, UTI %in% c(1))
        }

        return(df_aux)
    })



    output$mini_srag_plot <- renderPlot({
        render_srag_notification_card_plot(df_agg_dia)
    })

    output$mini_obito_plot <- renderPlot({
        render_srag_notification_card_plot(df_agg_obito)
    })

    output$mini_uti_plot <- renderPlot({
        render_srag_notification_card_plot(df_agg_uti)
    })

    output$mini_internacao_plot <- renderPlot({
        render_srag_notification_card_plot(df_agg_internacao)
    })

    output$seasonal_plot <- renderPlotly({
        render_seasonal_plot(output, input, df_filtered())
    })

    output$frequency_plot <- renderPlotly({
        render_frequency_plot(output, input, df_filtered())
    })

    output$subseries_plot <- renderPlotly({
        render_subseries_plot(output, input, df_filtered())
    })

    output$transformation_plot <- renderPlotly({
        render_transformation_plot(output, input, df_filtered())
    })

    output$transformation_plot2 <- renderPlotly({
        render_transformation_plot2(output, input, df_filtered())
    })

    data_menor <- reactive({
        get_data_menor(df_filtered())
    })

    # dt <- reactive({
    #     dataset_after_variance_transformation_base(output, input, df)
    # })

    grau <- reactive({
        input$tendency_degree_input
    
    })

    dt = reactiveVal(NULL)
    # dt_ts = reactiveVal(NULL)
    # dt_i_tendency = reactiveVal(NULL)
    
    
    observeEvent(input$confirm_filters, {
      dt(dataset_after_variance_transformation_base(output, input, df_filtered()))
    #   dt_i(dataset_after_variance_transformation2(output, input, dt, data_menor()))
    #   dt_ts(dataset_after_variance_transformation1(output, input, dt, data_menor()))
    #  dt_i_tendency(dataset_after_tendency_transformation(output, input, dt_i()))
    #   output$variance_plot <- renderPlotly({
    #     render_variance_plot(output, input, dt_i(), dt_ts())
    #   })
    # })
    # 
    # observeEvent(input$confirm_transformation, {
    #   output$tendency_plot <- renderPlotly({
    #     render_tendency_plot(output, input, dt_i_tendency(), dt_i())
    #   })
    # })
    # 
    # observeEvent(input$confirm_tendency, {
    #   output$decomposition_plot <- renderPlotly({
    #     render_decomposition_plot(output, input, dt_i_tendency())
    #   })
    # })
    # 
    # observeEvent(input$confirm_decomposition, {
    #   output$autocorrelation_plot <- renderPlotly({
    #     render_lag_plot_diff(output, input, decomposed_data())
    #   })
    })
    

    valores_ajustados_tendencia <- reactive({
        dataset_after_tendency_transformation(output, input, dt(), grau())
    })
#     dt <- reactive({
#         dataset_after_variance_transformation_base(output, input, df_filtered())
#     })
# 
#     dt_i <- reactive({
#         dataset_after_variance_transformation2(output, input, dt(), data_menor())
#     })
#     
#     dt_ts <- reactive({
#         dataset_after_variance_transformation1(output, input, dt(), data_menor())
#     })

    new_dt_ts_tend <- reactive({
        new_dt_ts_tend = dt() - valores_ajustados_tendencia()
    })

    



    output$tendency_plot_1 <- renderPlotly({
        render_tendency_plot_1(output, input, new_dt_ts_tend(), valores_ajustados_tendencia())
    })

    output$tendency_plot_2 <- renderPlotly({
        render_tendency_plot_2(output, input, new_dt_ts_tend())
    })



    # dt_i_tendency <- reactive({
    #     dataset_after_tendency_transformation(output, input, dt_i())
    # })
    
    valores_ajustados_sazo <- reactive({
        prepare_sazonality_plot(output, input, new_dt_ts_tend())
    })

    output$sazonality_plot_1 <- renderPlotly({
        render_sazonality_plot_1(output, input, new_dt_ts_tend(), valores_ajustados_sazo())
    })

    output$sazonality_plot_2 <- renderPlotly({
        render_sazonality_plot_2(output, input, new_dt_ts_tend(), valores_ajustados_sazo())
    })



    tsdata <- reactive({
        get_data_to_lag_plot(df_filtered())
    })

    monthly <- reactive({
        get_data_to_lag_plot2(tsdata())
    })

    output$lag_plot <- renderPlotly({
        if(input$lag_input == "Diario"){
            render_lag_plot(output, input, tsdata())
        }else{
            render_lag_plot3(output, input, monthly())
        }

    })

    output$lag_plot2 <- renderPlotly({
        if(input$lag_input == "Diario"){
            render_lag_plot2(output, input, tsdata())
        }else{
            render_lag_plot4(output, input, monthly())
        }

    })

    decomposed_data <- reactive({
        new_dt_ts_sazo = new_dt_ts_tend() - valores_ajustados_sazo()
    })
    

    output$autocorrelation_plot <- renderPlotly({
        render_lag_plot_diff(output, input, decomposed_data())
    })



    # observeEvent(input$show_filters, {
    #   showModal(filters_modal)
    # })


  router$server(input, output, session)


}

shinyApp(ui, server)