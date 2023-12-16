

map_month = c(
    "Jan" = 1, "Fev" = 2, "Mar" = 3, "Abr" = 4,
    "Mai" = 5, "Jun" = 6, "Jul" = 7, "Ago" = 8,
    "Set" = 9, "Out" = 10, "Nov" = 11, "Dez" = 12
)
map_day = c(

    "Seg" = "seg", "Ter" = "ter", "Qua" = "qua",
    "Qui" = "qui", "Sex" = "sex", "Sab" = "sáb",
    "Dom" = "dom"

)
ordem_dia_semana = c( "seg","ter","qua","qui","sex", "sáb", "dom")

render_seasonal_plot <- function(output, input, base_i){

    type_ <- input$ethnicity_type_filter_input

    mes_selecionados <- c()
    for (i in input$seasonal_window_months_filter_input){
      mes_selecionados <- c(mes_selecionados, map_month[i])
    }
    anos_selecionados <- c()
    for (i in input$seasonal_window_year_filter_input){
      anos_selecionados <- c(anos_selecionados, strtoi(i))
    }
    semana_selecionados <- c()
    for (i in input$seasonal_window_yearweek_filter_input){
      semana_selecionados <- c(semana_selecionados, strtoi(i))
    }
    dia_semana_selecionados <- c()
    for (i in input$seasonal_window_weekday_filter_input){
      dia_semana_selecionados <- c(dia_semana_selecionados, map_day[i])
    }

    
    # Filtre os dados pelos anos selecionados
    base_filtrada <- base_i[(ano %in% anos_selecionados) & (mes %in% mes_selecionados) & (dia_semana %in% dia_semana_selecionados) & (semana_ano %in% semana_selecionados) ]
    
    # Agrupe os dados conforme o periodo sazonal selecionado
    dados_plot <- switch(input$seasonal_type_input,
                        "Mês" = setorderv(base_filtrada[,.(CS_RACA=CS_RACA, SG_UF=SG_UF, Num_casos = .N, grupo = as.character(paste0(ano,'/',str_pad(mes,width = 2,side = 'left',pad = '0')))), by = .(mes,dia,ano)],cols = c('mes','dia'),order = c(1,1)),
                         "Semana" = setorderv(base_filtrada[,.(CS_RACA=CS_RACA, SG_UF=SG_UF, Num_casos = .N, dia_semana_num,grupo = as.character(paste0(ano,'/',str_pad(semana_ano,width = 2,side = 'left',pad = '0')))), by = .(semana_ano,dia_semana,ano)],cols = c('semana_ano','dia_semana_num'),order = c(1,1)),
                         "Ano" = setorderv(base_filtrada[,.(CS_RACA=CS_RACA, SG_UF=SG_UF, Num_casos = .N, grupo = as.character(ano)), by = .(ano,dia_ano)],cols = c('ano','dia_ano'),order = c(1,1))
    )
    
    # definindo os titulos do eixo horizontal e a legenda do titulo
    if (input$seasonal_type_input == "Mês") {
      legenda_titulo <- "Mês"
      legenda_x <- "Dia do mês"
    } else if (input$seasonal_type_input == "Semana") {
      legenda_titulo <- "Dia da Semana"
      legenda_x <- "Dia da semana"
    } else {
      legenda_titulo <- "Ano"
      legenda_x <- "Dia do ano"
    }
    dados_plot$dia_semana <- factor(dados_plot$dia_semana, levels = ordem_dia_semana)
    # Criando o grafico iterativo com plot_ly

    if (type_ == "Agrupada"){
      plot_ly(data = dados_plot, x = ~switch(input$seasonal_type_input,
                                            "Mês" = dia,
                                            "Semana" = dia_semana,
                                            "Ano" = dia_ano), y = ~Num_casos, type = "scatter", mode = "lines",color =  ~grupo
      ) %>%
        layout(
          title = paste("Gráfico sazonal por", input$seasonal_type_input,sep = " "),
          xaxis = list(title = legenda_x),
          yaxis = list(title = "Casos"),
          legend = list(title = legenda_titulo),
          margin = c(l=50, r=50, b=100, t=100, pad=4)
        )
    }else if (type_ == "Individual por Estado"){
       plot_ly(data = dados_plot, x = ~switch(input$seasonal_type_input,
                                              "Mês" = dia,
                                              "Semana" = dia_semana,
                                              "Ano" = dia_ano), y = ~Num_casos, type = "scatter", mode = "lines",color =  ~interaction(grupo, SG_UF, sep='-')
        ) %>%
          layout(
            title = paste("Gráfico sazonal por", input$seasonal_type_input,sep = " "),
            xaxis = list(title = legenda_x),
            yaxis = list(title = "Casos"),
            legend = list(title = legenda_titulo),
            margin = c(l=50, r=50, b=100, t=100, pad=4)
          )
    }else if(type_ == "Individual por Cor/Raça/Etnia"){
      plot_ly(data = dados_plot, x = ~switch(input$seasonal_type_input,
                                              "Mês" = dia,
                                              "Semana" = dia_semana,
                                              "Ano" = dia_ano), y = ~Num_casos, type = "scatter", mode = "lines",color =  ~interaction(grupo, CS_RACA, sep='-')
        ) %>%
          layout(
            title = paste("Gráfico sazonal por", input$seasonal_type_input,sep = " "),
            xaxis = list(title = legenda_x),
            yaxis = list(title = "Casos"),
            legend = list(title = legenda_titulo),
            margin = c(l=50, r=50, b=100, t=100, pad=4)
          )
    }


}