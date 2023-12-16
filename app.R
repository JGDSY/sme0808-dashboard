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
library(investr)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(stats)


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
source("./src/server/model_fit.R")



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



df_agg_dia <- df[, sum(value), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_obito <- df[, sum(obito), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_obito <- na.omit(df_agg_obito)
df_agg_internacao <- df[, sum(HOSPITAL), by = DT_NOTIFIC][order(DT_NOTIFIC)]
df_agg_uti <- df[, sum(UTI), by = DT_NOTIFIC][order(DT_NOTIFIC)]


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

    dt <- reactive({
        dataset_after_variance_transformation_base(output, input, df_filtered())
    })

    grau <- reactive({
        input$tendency_degree_input
    
    })

    # dt = reactiveVal(NULL)
    # dt_ts = reactiveVal(NULL)
    # dt_i_tendency = reactiveVal(NULL)
    
    
    observeEvent(input$confirm_filters, {
    #   dt(dataset_after_variance_transformation_base(output, input, df_filtered()))
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
        if (input$transformation_tendency2 == "Box-Cox"){
            new_dt_ts_tend = valores_ajustados_tendencia()
        }else{
            new_dt_ts_tend = dt() - valores_ajustados_tendencia()
        }
    })

    



    output$tendency_plot_1 <- renderPlotly({
        print("dddddddddddd")
        a<-render_tendency_plot_1(output, input, new_dt_ts_tend(), valores_ajustados_tendencia())
        print("aaaaaaaaaaaaaaaaaaaa")
         render_tendency_plot_2(output, input, new_dt_ts_tend())
         print("eeeeeeeeeeee")
        return(a)
    })

    # output$tendency_plot_2 <- renderPlotly({
    #     render_tendency_plot_2(output, input, new_dt_ts_tend())
    # })



    # dt_i_tendency <- reactive({
    #     dataset_after_tendency_transformation(output, input, dt_i())
    # })
    
    valores_ajustados_sazo <- reactive({
        prepare_sazonality_plot(output, input, new_dt_ts_tend())
    })

    output$sazonality_plot_1 <- renderPlotly({

        tryCatch({
             render_sazonality_plot_1(output, input, new_dt_ts_tend(), valores_ajustados_sazo())
        },
        error = function(cond){

        })

        
    })

    output$sazonality_plot_2 <- renderPlotly({

        tryCatch({
             render_sazonality_plot_2(output, input, new_dt_ts_tend(), valores_ajustados_sazo())
        },
        error = function(cond){

        })


        
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
        if (input$transformation_sazonalidade == "Diferenciação"){
            dd(input$transformation_sazonalidade_diff)
            new_dt_ts_sazo =  valores_ajustados_sazo()
        }
        else{
            new_dt_ts_sazo = new_dt_ts_tend() - valores_ajustados_sazo()
        }
        
    })
    

    output$autocorrelation_plot <- renderPlotly({
        render_lag_plot_diff(output, input, decomposed_data())
    })

    output$autocorrelation_plot2 <- renderPlotly({
        render_lag_plot_diff2(output, input, decomposed_data())
    })

    output$stationary_test <- renderText({
        test = tseries::kpss.test(decomposed_data())

        if(test$p.value <= 0.05){
            runjs('document.getElementById("recommendation_box").style.backgroundColor = "#f44336";')
            return(
                
                paste0(paste0("Pelo teste KPSS, a serie não é estacionaria com valor-p=", test$p.value), ". Refaça os ajustes necessarios")
            )
        }else{
            runjs('document.getElementById("recommendation_box").style.backgroundColor = "#f44336";')
            return(paste0("Pelo teste KPSS, a serie é estacionaria com valor-p=", test$p.value))
        }
    })

    p = reactiveVal(NULL)
    qq = reactiveVal(NULL)
    dd = reactiveVal(NULL)

    output$recomendation_model <- renderText({
        check <- function(val, n){
            up = 2/sqrt(n)

            for(i in c(1:30)){
                if(abs(val$acf[i]) < up){
                    if(i < 15){

                        return(i)
                    }
                    else{
                        return(0)
                    }
                    
                }
            }
            return(0)
        }


        ma = check(
            acf(decomposed_data(), pl=F,lag=30, na.action=na.pass), length(decomposed_data())
        )
        
        ar = check(
            pacf(decomposed_data(), pl=F,lag=30, na.action=na.pass), length(decomposed_data())
        )

        p(ma)
        qq(ar)

        if(input$transformation_sazonalidade == "Diferenciação"){
             updateNumericInput(session, "ARIMA_i", value=input$transformation_sazonality_diff)
        }else{
             updateNumericInput(session, "ARIMA_i", value=0)
        }
       

        

        if (ma != 0 && ar != 0){
            ma = ma - 1
            updateNumericInput(session, "ARIMA_p", value=ar)
            updateNumericInput(session, "ARIMA_q", value=ma)
            if(input$transformation_sazonalidade == "Diferenciação"){
                return(
                paste0(paste0(paste0(
                    paste0(
                        paste0("Recomendamos que você use um modelo ARIMA com p=", ma),
                        " e q="
                    ),
                    ar
                ),""), "")
            )
            }
            else{
                return(
                paste0(
                    paste0(
                        paste0("Recomendamos que você use um modelo ARIMA com p=", ma),
                        " e q="
                    ),
                    ar
                )
            )
            }
            
        } else if (ma == 0 && ar != 0){
            updateNumericInput(session, "ARIMA_p", value=ar)
            updateNumericInput(session, "ARIMA_q", value=0)
            return(paste0("Recomendamos que você use um modelo AR com p=", ar))
        } else if (ma != 0 && ar == 0){
            ma = ma - 1
            updateNumericInput(session, "ARIMA_p", value=0)
            updateNumericInput(session, "ARIMA_q", value=ma)
            return(paste0("Recomendamos que você use um modelo MA com q=", ma))
        } else if (ma ==0 && ar == 0){
            return("Não conseguimos recomendar nenhum modelo. Refaça as transformações.")
        }


        

        
    })



    # observeEvent(input$show_filters, {
    #   showModal(filters_modal)
    # })

    observeEvent(input$confirm_autocorrelation,{
        updateTabsetPanel(session, "tabs", "Modelos preditivos")
    })

    best_fit <- reactiveVal(0)
    adjusted_fit <- reactiveVal(0)
    fittedd <- reactiveVal(F)

    

    observeEvent(input$model_selection,{
        
        train_series <- get_train_timeseries_split(output, input, decomposed_data)
        test_series <- get_test_timeseries_split(output, input, decomposed_data)

        best <- auto.arima(decomposed_data())
        fit <- model_fit(output, input, train_series) 

        best_fit(best)
        adjusted_fit(fit)
        fittedd(T)

        
    })

    forecasted_data <- reactive({
        train_series <- get_train_timeseries_split(output, input, decomposed_data=dt)
        get_forecast_data(output, input, adjusted_fit(), valores_ajustados_tendencia(), valores_ajustados_sazo(), train_series)
    })

    output$forecast <- renderPlot({
        test_series <- get_test_timeseries_split(output, input,  dt)
        train_series <- get_train_timeseries_split(output, input, decomposed_data=dt)
        # test_series <- get_test_timeseries_split(output, input, decomposed_data)
        get_inverse_transformed_plot(
            output,
            input,
            adjusted_fit(),
            valores_ajustados_tendencia(),
            valores_ajustados_sazo(),
            test_series, decomposed_data(),
            train_series
        )
    })

    output$forecast_metrics <- renderText({
        if (input$test_steps_slider != 0){
            data <- forecasted_data()
            test_series <- get_test_timeseries_split(output, input, decomposed_data)

        }else {

        }
        
    })

     output$residual_text <- renderText({
        get_residual_hypothesis_testing(output, input, adjusted_fit())
     })

    output$residuals <- renderPlotly({
        train_series <- get_train_timeseries_split(output, input, decomposed_data)
        get_residual_acf_plot(output, input, adjusted_fit(), train_series)
    })

    output$table_metrics <- renderTable({
        get_fit_metrics_table(output, input, adjusted_fit(), best_fit(), fittedd())
    })


    model_state <- reactiveVal(FALSE)


    observeEvent(input$model_selection, {
        if(model_state() == F){
            model_state(T)
            shinyjs::toggle("tab_models")
        }
      
    })

    observeEvent(input$model_reset, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$model_choice, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$test_steps_slider, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$ARIMA_q, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$ARIMA_p, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$ARIMA_i, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    ###
    observeEvent(input$transformation_tendency2, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_tendency, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$tendency_degree_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$confirm_transformation, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonalidade, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })
    observeEvent(input$transformation_random, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonality_mm, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    ###

    observeEvent(input$transformation_sazonalitya, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonalityb, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonalityc, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonalityd, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$transformation_sazonalityfourrier, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$confirm_sazonality, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })


    observeEvent(input$confirm_autocorrelation, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    ##

    observeEvent(input$analysis_type_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$age_window_filter_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$year_filter_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$ethnicity_type_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$sickness_filter_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$state_filter_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })

    observeEvent(input$lag_input, {
      if(model_state() == T){
            model_state(F)
            shinyjs::toggle("tab_models")
        }
    })



    



  router$server(input, output, session)


}

shinyApp(ui, server)