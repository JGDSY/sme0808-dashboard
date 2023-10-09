

switch_transformations_individual <- function(df_ts, input, x_name){

    if (input$frequency_average_type_input == "Média Movel"){
        df_ts <- df_ts[order(-get(x_name))]
        df_ts[, V3 := frollmean(V1, input$frequency_moving_average_window_input)]
        df_ts <- df_ts[order(-get(x_name))]
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V3, name = "Notificações")
    }else if (input$frequency_average_type_input == "Diferenciação"){
        get(x_name)=df_ts$get(x_name)[1:(length(df_ts$get(x_name))-input$frequency_differentiation_order_input)]
        V3=diff(df_ts$V1, lag=input$frequency_differentiation_order_input)
        df_ts <- data.table(get(x_name), V3)[order(-get(x_name))]
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V3, name = "Notificações")
    }else {
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V1, name = "Notificações")
    }
    return(fig)

}

switch_transformations_grouped <- function(df_ts, input, x_name, group_name){

    if (input$frequency_average_type_input == "Média Movel"){
        df_ts <- df_ts[order(-get(x_name))]
        df_ts[, V3 := frollmean(V1, input$frequency_moving_average_window_input), by=list(get(group_name))]
        df_ts <- df_ts[order(-get(x_name))]
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V3, name = ~get(group_name), color=~get(group_name))
    }else if (input$frequency_average_type_input == "Diferenciação"){
        df_ts <- df_ts[order(-get(x_name))]
        df_ts[, V3 := rollapplyr(V1, input$frequency_differentiation_order_input+1, function(x){diff(x, lag=input$frequency_differentiation_order_input)}, na.pad=TRUE), by=get(group_name)]
        df_ts <- na.omit(df_ts)
        df_ts <- df_ts[order(-get(x_name))]
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V3, name = ~get(group_name), color=~get(group_name))
    }else {
        fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
        add_trace(x = ~get(x_name), y = ~V1, name = ~get(group_name), color=~get(group_name))
    }
    return(fig)

}


render_frequency_plot <- function(output, input, df){
    input_value = input$frequency_type_input

    type_ <- input$ethnicity_type_filter_input
    

    if (input_value == "Dia"){
        xaxis = "Dias"

        

        if (type_ == "Agrupada"){
            df_ts <- df[, sum(value), by = DT_NOTIFIC][order(-DT_NOTIFIC)]
            fig <- switch_transformations_individual(df_ts, input, "DT_NOTIFIC")
        }else if (type_ == "Individual por Estado"){
            df_ts <- df[, sum(value), by = list(DT_NOTIFIC, SG_UF)][order(-DT_NOTIFIC)]
            fig <- switch_transformations_grouped(df_ts, input, "DT_NOTIFIC", "SG_UF")
        }else if(type_ == "Individual por Cor/Raça/Etnia"){
            df_ts <- df[, sum(value), by = c(DT_NOTIFIC, CS_RACA)][order(-DT_NOTIFIC)]
            fig <- switch_transformations_grouped(df_ts, input, "DT_NOTIFIC", "CS_RACA")
        }
        

    }else if (input_value == "Semana"){
        xaxis = "Semanas"

        ethnicity <- input$ethnicity_type_filter_input
        uf <- input$ethnicity_type_filter_input

        

        if (type_ == "Agrupada"){
            df_ts <- df[, sum(value), by = week_ano][order(-week_ano)]
            fig <- switch_transformations_individual(df_ts, input, "week_ano")
        }
        else if (type_ == "Individual por Estado"){
            df_ts <- df[, sum(value), by = list(week_ano, SG_UF)][order(-week_ano)]
            fig <- switch_transformations_grouped(df_ts, input, "week_ano", "SG_UF")

        }else if(type_ == "Individual por Cor/Raça/Etnia"){
            df_ts <- df[, sum(value), by = list(week_ano, CS_RACA)][order(-week_ano)]
            fig <- switch_transformations_grouped(df_ts, input, "week_ano", "CS_RACA")
        }
        

        

    }else {
        xaxis = "Meses"

        ethnicity <- input$ethnicity_type_filter_input
        uf <- input$ethnicity_type_filter_input


        if (type_ == "Agrupada"){
            df_ts <- df[, sum(value), by = ano_mes][order(-ano_mes)]
            fig <- switch_transformations_individual(df_ts, input, "week_ano")
            
        }
        else if (type_ == "Individual por Estado"){
            df_ts <- df[, sum(value), by = list(ano_mes, SG_UF)][order(-ano_mes)]
            fig <- switch_transformations_grouped(df_ts, input, "ano_mes", "SG_UF")
            
        }else if(type_ == "Individual por Cor/Raça/Etnia"){
            df_ts <- df[, sum(value), by = list(ano_mes, CS_RACA)][order(-ano_mes)]
            fig <- switch_transformations_grouped(df_ts, input, "ano_mes", "CS_RACA")
           
        }
        
    }
    fig <- fig %>%
        layout(
                showlegend = TRUE,
                margin = c(l=50, r=50, b=100, t=100, pad=4),
                title = paste("Grafico da Serie Temporal de Notificações por", input_value),
                xaxis = list(title=xaxis),
                yaxis = list(title="Casos")
            )

    return(fig)
}