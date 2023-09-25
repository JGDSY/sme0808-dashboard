
render_frequency_plot <- function(output, input, df){
    input_value = input$frequency_type_input

    df_aux <- df

    

    if (input_value == "Dia"){
        xaxis = "Dias"

        type_ <- input$ethnicity_type_filter_input

        if (type_ == "Agrupada"){
            df_ts <- aggregate(df_aux["value"], by = df_aux["DT_NOTIFIC"], sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~DT_NOTIFIC, y = ~value, name = "Notificações")
        }
        else if (type_ == "Individual por Estado"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["DT_NOTIFIC"], df_aux["SG_UF"]), sum)
            fig <- plot_ly(
                df_ts, type = "scatter", mode = "lines"
            ) %>%
             add_trace(x = ~DT_NOTIFIC, y = ~value, name = ~SG_UF, color = ~SG_UF)
        }else if(type_ == "Individual por Cor/Raça/Etnia"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["DT_NOTIFIC"], df_aux["CS_RACA"]), sum)
            fig <- plot_ly(
                df_ts, type = "scatter", mode = "lines"
            ) %>%
             add_trace(x = ~DT_NOTIFIC, y = ~value, name = ~CS_RACA, color = ~CS_RACA)
        }
        

    }else if (input_value == "Semana"){
        xaxis = "Semanas"

        ethnicity <- input$ethnicity_type_filter_input
        uf <- input$ethnicity_type_filter_input

        if (ethnicity == "Agrupada" & uf == "Agrupada"){
            df_ts <- aggregate(df_aux["value"], by = df_aux["week_ano"], sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~week_ano, y = ~value, name = "Notificações")
        }
        else if (ethnicity == "Agrupada" & uf == "Individual"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["week_ano"], df_aux["SG_UF"]), sum)
            print(head(df_ts))
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~week_ano, y = ~value, color = ~SG_UF)
        }else if(ethnicity == "Individual" & uf == "Agrupada"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["week_ano"], df_aux["CS_RACA"]), sum)
            print(head(df_ts))
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~week_ano, y = ~value, color = ~CS_RACA)
        }else{
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["week_ano"], df_aux["RACA_UF"]), sum)
            print(head(df_ts))
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~week_ano, y = ~value, color = ~RACA_UF)
        }
        

        

    }else {
        xaxis = "Meses"

        ethnicity <- input$ethnicity_type_filter_input
        uf <- input$ethnicity_type_filter_input

        if (ethnicity == "Agrupada" & uf == "Agrupada"){
            df_ts <- aggregate(df_aux["value"], by = df_aux["ano_mes"], sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~ano_mes, y = ~value, name = "Notificações")
            
        }
        else if (ethnicity == "Agrupada" & uf == "Individual"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["ano_mes"], df_aux["SG_UF"]), sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~ano_mes, y = ~value, color = ~SG_UF)
            
        }else if(ethnicity == "Individual" & uf == "Agrupada"){
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["ano_mes"], df_aux["CS_RACA"]), sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~ano_mes, y = ~value, color = ~CS_RACA)
           
        }else{
            df_ts <- aggregate(df_aux["value"], by = c(df_aux["ano_mes"], df_aux["RACA_UF"]), sum)
            fig <- plot_ly(df_ts, type = "scatter", mode = "lines") %>%
            add_trace(x = ~ano_mes, y = ~value, color = ~RACA_UF)
            
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