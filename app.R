library(shiny)
library(leaflet)
library(plotly)
library(rgdal)
library(dplyr)
library(data.table)
library(stringr)
library(bslib)

Sys.setlocale("LC_ALL", "pt_PT.UTF-8")


source("./src/components/route.R")
source("./src/server/seasonal_plot.R")
source("./src/server/cards_plot.R")
source("./src/server/frequency_plot.R")
source("./src/server/map.R")


ui <- bootstrapPage(div(router$ui))

print("Starting data loading and processing")

df <- fread("./data/2019_2020_SRAG_v2_processed.csv")

# df$value <- 1
# df$data_referencia =as.Date(df$DT_NOTIFIC,'%d/%m/%Y')
# df$DT_NOTIFIC <- as.Date(df$DT_NOTIFIC, "%d/%m/%Y")
# df$ano = as.numeric(format(df$data_referencia,'%Y'))
# df$NU_IDADE_N = as.numeric(df$NU_IDADE_N)
# df$mes = as.numeric(format(df$data_referencia,'%m'))
# df$dia = as.numeric(format(df$data_referencia,'%d'))
# df$dia_ano = as.numeric(df$data_referencia - as.Date(paste0(df$ano -1,'-','12','-','31')))
# df$dia_semana = weekdays(df$data_referencia,abbreviate = T)
# df$dia_semana_num = NA
# ordem_dia_semana = c( "seg","ter","qua","qui","sex", "sáb", "dom")
# df[, dia_semana_num := match(dia_semana, ordem_dia_semana)]
# df$semana_ano = as.numeric(ceiling(df$dia_ano/7))
# df$ano_mes <- format(as.Date(df$DT_NOTIFIC), "%Y-%m")
# df$week_ano <- format(as.Date(paste(df$ano, df$semana_ano, 1, sep="-"), "%Y-%U-%u"))
# df$RACA_UF <- paste0(df$CS_RACA,"-",df$SG_UF)

etinia_map = c(
    "Branca" = 1, "Preta" = 2, "Amarela" = 3,
    "Parda" = 4, "Indigena" = 5, "Não Especificado" = 9
)


gc()


df_df <- df
class(df_df) <- class(as.data.frame(df_df))


df_agg_dia <- aggregate(df_df["value"], by=df_df["DT_NOTIFIC"], sum)




print("Ending data loading and processing")


server <- function(input, output, session) {

    etinias_input <- reactive({
        return(input$ethnicity_type_input)
    })
    

    df_df_filtered <- reactive({

        mes_selecionados <- c()
        for (i in input$seasonal_window_months_filter_input){
            mes_selecionados <- c(mes_selecionados, map_month[i])
        }

        semana_selecionados <- c()
        for (i in input$seasonal_window_yearweek_filter_input){
            semana_selecionados <- c(semana_selecionados, strtoi(i))
        }
        dia_semana_selecionados <- c()
        for (i in input$seasonal_window_weekday_filter_input){
            dia_semana_selecionados <- c(dia_semana_selecionados, map_day[i])
        }

        age_window <- c(input$age_window_filter_input[1]:input$age_window_filter_input[2])
        year_window <- input$seasonal_window_year_filter_input
        df_aux <- filter(df_df, CS_RACA %in% input$ethnicity_type_input)
        df_aux <- filter(df_aux, NU_IDADE_N %in% age_window)
        df_aux <- filter(df_aux, ano %in% year_window)
        df_aux <- filter(df_aux, mes %in% mes_selecionados)
        df_aux <- filter(df_aux, dia_semana %in% dia_semana_selecionados)
        df_aux <- filter(df_aux, semana_ano %in% semana_selecionados)

        return(df_aux)
    })

    df_filtered <- reactive({
        mes_selecionados <- c()
        for (i in input$seasonal_window_months_filter_input){
            mes_selecionados <- c(mes_selecionados, map_month[i])
        }

        semana_selecionados <- c()
        for (i in input$seasonal_window_yearweek_filter_input){
            semana_selecionados <- c(semana_selecionados, strtoi(i))
        }
        dia_semana_selecionados <- c()
        for (i in input$seasonal_window_weekday_filter_input){
            dia_semana_selecionados <- c(dia_semana_selecionados, map_day[i])
        }

        age_window <- c(input$age_window_filter_input[1]:input$age_window_filter_input[2])
        year_window <- input$seasonal_window_year_filter_input
        df_aux <- filter(df, CS_RACA %in% input$ethnicity_type_input)
        df_aux <- filter(df_aux, NU_IDADE_N %in% age_window)
        df_aux <- filter(df_aux, ano %in% year_window)
        df_aux <- filter(df_aux, mes %in% mes_selecionados)
        df_aux <- filter(df_aux, dia_semana %in% dia_semana_selecionados)
        df_aux <- filter(df_aux, semana_ano %in% semana_selecionados)
        return(df_aux)
    })



    output$mini_srag_plot <- renderPlot({
        render_srag_notification_card_plot(df_agg_dia)
    })

    output$seasonal_plot <- renderPlotly({
        render_seasonal_plot(output, input, df_filtered())
    })

    output$frequency_plot <- renderPlotly({
        render_frequency_plot(output, input, df_df_filtered())
    })

    output$map <- renderLeaflet({
        render_map(output, input)
    })

    observeEvent(input$map_shape_click, {
        p <- input$map_shape_click
        print(p)
    })


  router$server(input, output, session)


}

shinyApp(ui, server)