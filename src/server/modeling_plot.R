
get_data_menor <- function(base_i){
    menor_data = min(base_i$data_referencia)
    data_menor = c(as.numeric(format(menor_data,'%Y')),
               as.numeric(format(menor_data,'%m')),
               as.numeric(format(menor_data,'%d')))
    return(data_menor)
}


dataset_after_variance_transformation_base <- function(output, input, df){

    menor_data = min(df$data_referencia)
    data_menor = c(as.numeric(format(menor_data,'%Y')),
               as.numeric(format(menor_data,'%m')),
               as.numeric(format(menor_data,'%d')))

    if(input$frequency_type_input == "Dia"){
        freq = 365
        dt = df[,.(total = .N),by = data_referencia]
        dt = dt[order(data_referencia)]
    } else if(input$frequency_type_input == "Semanal"){
        freq = 365/7
        dt = df[,.(total = .N),by = week_ano]
        dt = dt[order(week_ano)]
    } else {
        freq = 365/31
        dt = df[,.(total = .N),by = ano_mes]
        dt = dt[order(ano_mes)]
    }
    

    

    
    dt_ts = ts(dt$total,start = c(data_menor[1],data_menor[2],data_menor[3]),frequency = freq)

    return(dt_ts)

    

}


dataset_after_tendency_transformation <- function(output, input, dt_ts, grau){


    if(input$transformation_tendency == "Log"){
        dados <- log(dt_ts)
    } else if(input$transformation_tendency == "Raiz Quadrada"){
        dados <- sqrt(dt_ts)
    } else if(input$transformation_tendency == "Polinomio"){
        
        dados <- dt_ts
    } else {
        dados <- dt_ts
    }

    dados_transformados <- dados


    if(input$transformation_tendency2 != "Não Aplicar"){

        if(input$transformation_tendency2 == "Polinomio"){
            print(grau)
            print(typeof(grau))
            modelo_tendencia = tslm(dt_ts ~trend + poly(as.vector(time(dt_ts)), 2))
        } else if(input$transformation_tendency2 == "Linear Por Partes"){
            
            # Encontrar pontos de mudança usando o método BinSeg
            pontos_corte <- cpts(cpt.mean(dt_ts, method = "BinSeg"))
            
            # Ajustar modelos tslm para cada segmento da série
            modelos <- list()
            valores_ajustados_tendencia = c()
            lista_segmentos <- list()
            # Loop para criar modelos usando tslm para cada segmento
            for (i in 1:length(pontos_corte)) {
            if (i == 1) {
                # Primeiro segmento: do início até o primeiro ponto de mudança
                segmento <- window(dt_ts, end = time(dt_ts)[pontos_corte[i]])
            } else {
                # Segmentos intermediários: entre os pontos de mudança
                segmento <- window(dt_ts, start = time(dt_ts)[pontos_corte[i - 1] + 1], end = time(dt_ts)[pontos_corte[i]])
            }
            
            # Ajustar modelos tslm para cada segmento
            modelos[[i]] <- tslm(segmento ~ trend)
            previsao_segmento = fitted(modelos[[i]])
            valores_ajustados_tendencia = c(valores_ajustados_tendencia,previsao_segmento)
            dados_segmento <- data.frame(
                Tempo = time(segmento),
                Original = as.vector(segmento),
                Ajustado = previsao_segmento
            )
            
            # Armazenar os dados do segmento na lista
            lista_segmentos[[i]] <- dados_segmento
            
            
            }
            segmento_final = window(dt_ts, start = time(dt_ts)[pontos_corte[i] + 1])
            modelos[[i+1]] <- tslm(segmento_final ~ trend)
            previsao_segmento = fitted(modelos[[i+1]])
            valores_ajustados_tendencia = c(valores_ajustados_tendencia,previsao_segmento)
            dados_segmento <- data.frame(
            Tempo = time(segmento_final),
            Original = as.vector(segmento_final),
            Ajustado = previsao_segmento
            )
            
            # Armazenar os dados do segmento na lista
            lista_segmentos[[i+1]] <- dados_segmento
            dados_todos_segmentos <- bind_rows(lista_segmentos, .id = "Segmento")
            

        } else if(input$transformation_tendency2 == "Media Movel"){
            rollavg = input$transformation_tendency2_mm

            modelo_sazonalidade = stats::filter(
                dados_transformados, filter = rep(1/rollavg, rollavg), method = 'convolution', sides = 1
            )

            return(modelo_sazonalidade)

        }else if(input$transformation_tendency2 == "Box-Cox"){
            box = BoxCox(dados_transformados, "auto")
            input$lambda = box[,"attr"]
            modelo_tendencia = tslm(dados_transformados ~ BoxCox(dados_transformados, "auto"))

        }
        
        else {
            modelo_tendencia = tslm(dados_transformados ~ trend)
        }


        if(input$transformation_tendency2 != "Linear Por Partes"){
            #valores_ajustados = predict(modelo_tendencia, newdata = dados_transformados)
            valores_ajustados_tendencia = fitted(modelo_tendencia)
            

            if(input$transformation_tendency == "Log"){
                valores_ajustados_tendencia <- exp(fitted(modelo_tendencia))
            } else if(input$transformation_tendency == "Raiz Quadrada"){
                valores_ajustados_tendencia <- (fitted(modelo_tendencia)^2)
            }
            

        }
        
    } else{
        valores_ajustados_tendencia = 0
    }

    
    

    return(valores_ajustados_tendencia)

}

render_tendency_plot_1 <- function(output, input, dt_ts, valores_ajustados_tendencia){

    if(input$transformation_tendency2 != "Linear Por Partes"){


            dados_tendencia <- data.frame(
                Tempo = time(dt_ts),
                Original = as.vector(dt_ts),
                Ajustado = valores_ajustados_tendencia
            )
            
            fig <- ggplot(dados_tendencia, aes(x = Tempo)) +
            geom_line(aes(y = Original), color = "black") +
            geom_line(aes(y = Ajustado), color = "red", linetype = "dashed") +
            labs(title = "Série Original vs. Ajustada") +
            theme_minimal()
    }else if(input$transformation_tendency2 == "Linear Por Partes"){
            # Encontrar pontos de mudança usando o método BinSeg
            pontos_corte <- cpts(cpt.mean(dt_ts, method = "BinSeg"))
            
            # Ajustar modelos tslm para cada segmento da série
            modelos <- list()
            valores_ajustados_tendencia = c()
            lista_segmentos <- list()
            # Loop para criar modelos usando tslm para cada segmento
            for (i in 1:length(pontos_corte)) {
            if (i == 1) {
                # Primeiro segmento: do início até o primeiro ponto de mudança
                segmento <- window(dt_ts, end = time(dt_ts)[pontos_corte[i]])
            } else {
                # Segmentos intermediários: entre os pontos de mudança
                segmento <- window(dt_ts, start = time(dt_ts)[pontos_corte[i - 1] + 1], end = time(dt_ts)[pontos_corte[i]])
            }
            
            # Ajustar modelos tslm para cada segmento
            modelos[[i]] <- tslm(segmento ~ trend)
            previsao_segmento = fitted(modelos[[i]])
            valores_ajustados_tendencia = c(valores_ajustados_tendencia,previsao_segmento)
            dados_segmento <- data.frame(
                Tempo = time(segmento),
                Original = as.vector(segmento),
                Ajustado = previsao_segmento
            )
            
            # Armazenar os dados do segmento na lista
            lista_segmentos[[i]] <- dados_segmento
            
            
            }
            segmento_final = window(dt_ts, start = time(dt_ts)[pontos_corte[i] + 1])
            modelos[[i+1]] <- tslm(segmento_final ~ trend)
            previsao_segmento = fitted(modelos[[i+1]])
            valores_ajustados_tendencia = c(valores_ajustados_tendencia,previsao_segmento)
            dados_segmento <- data.frame(
            Tempo = time(segmento_final),
            Original = as.vector(segmento_final),
            Ajustado = previsao_segmento
            )
            
            # Armazenar os dados do segmento na lista
            lista_segmentos[[i+1]] <- dados_segmento
            dados_todos_segmentos <- bind_rows(lista_segmentos, .id = "Segmento")
            
            # Plotar todos os segmentos ajustados juntos
            fig<-ggplot(dados_todos_segmentos, aes(x = Tempo)) +
            geom_line(aes(y = Original), color = "black") +
            geom_line(aes(y = Ajustado, color = as.factor(Segmento)), linetype = "dashed") +
            labs(title = "Série Original vs. Ajustada - Todos os Segmentos") +
            theme_minimal()
    }

    fig

}

render_tendency_plot_2 <- function(output, input, new_dt_ts_tend){

    autoplot(new_dt_ts_tend)


}



prepare_sazonality_plot <- function(output, input, new_dt_ts_tend, seasonal_inv){

    if (input$transformation_sazonalidade == 'Senoide') {
            seus_dados_df <- data.frame(x = time(new_dt_ts_tend), y = as.vector(new_dt_ts_tend))
            
            chute <- FALSE  # Define se o usuário deseja um chute inicial para os parâmetros

            if(input$transformation_random == "Heuristica Padrão"){
                valor_inicial_a <- max(new_dt_ts_tend) - min(new_dt_ts_tend)
                valor_inicial_b <- 2 * pi
                valor_inicial_c <- pi
                valor_inicial_d <- (max(new_dt_ts_tend) + min(new_dt_ts_tend))/2
            }else{
                valor_inicial_a <- input$transformation_sazonalitya
                valor_inicial_b <- input$transformation_sazonalityb
                valor_inicial_c <- input$transformation_sazonalityc
                valor_inicial_d <- input$transformation_sazonalityd
            }
                       
            
            modelo_sazonalidade <- nls(y ~ a * sin(b * x + c) + d, 
                                        data = seus_dados_df, 
                                        start = list(a = valor_inicial_a, b = valor_inicial_b, c = valor_inicial_c, d = valor_inicial_d),
                                        algorithm = "port")

            

            seasonal_inv$data = modelo_sazonalidade

            
        
    } else if (input$transformation_sazonalidade == 'Fourier') {
            #max_termos senoidais (pedir para o usuário)
            max_k = input$transformation_sazonalityfourrier
            periodo <- frequency(new_dt_ts_tend)
            
            # Definir K para não ultrapassar a metade do período -1 (para garantir que não ultrapasse)
            K <- min(max_k, floor(periodo / 2) - 1)
            
            # Criar manualmente os termos senoidais de Fourier
            termos_fourier <- matrix(0, nrow = length(new_dt_ts_tend), ncol = 2 * K)
            for (j in 1:K) {
                termos_fourier[, 2 * j - 1] <- sin(2 * pi * j * time(new_dt_ts_tend) / periodo)
                termos_fourier[, 2 * j] <- cos(2 * pi * j * time(new_dt_ts_tend) / periodo)
            }
            
            modelo_sazonalidade <- lm(new_dt_ts_tend ~ termos_fourier)

            modelo_sazonalidade_inv <- lm(termos_fourier ~ new_dt_ts_tend)
            seasonal_inv$data = modelo_sazonalidade_inv
        
    } else if (input$transformation_sazonalidade == "Diferenciação"){
        lag_dif = input$transformation_sazonality_diff
        modelo_sazonalidade = diff(new_dt_ts_tend, differences = lag_dif)

        seasonal_inv$data = modelo_sazonalidade_inv

        return(modelo_sazonalidade)

    }else if (input$transformation_sazonalidade == "Media Movel"){
        rollavg = input$transformation_sazonality_mm

        modelo_sazonalidade = stats::filter(
            new_dt_ts_tend, filter = rep(1/rollavg, rollavg), method = 'convolution', sides = 1
        )

        return(modelo_sazonalidade)

    } else {
        return(0)
    }


    valores_ajustados_sazo <- fitted(modelo_sazonalidade)
    
}

render_sazonality_plot_1 <- function(output, input, new_dt_ts_tend, valores_ajustados_sazo){
    if (input$transformation_sazonalidade == "Diferenciação"){

        title2 = 'Série Diferenciada'
        ytitle2 = 'Valores diferenciados'
        plot2 = ggplotly(autoplot(valores_ajustados_sazo)) %>% 
        layout(yaxis = list(title = ytitle2))
    }else {
        dados_sazo <- data.frame(
            Tempo = time(new_dt_ts_tend),
            Original = as.vector(new_dt_ts_tend),
            Ajustado = valores_ajustados_sazo
        )
        ggplot(dados_sazo, aes(x = Tempo)) +
        geom_line(aes(y = Original), color = "black") +
        geom_line(aes(y = Ajustado), color = "red", linetype = "dashed") +
        labs(title = "Série Original vs. Ajustada") +
        theme_minimal()
        
    }
    

    
}


render_sazonality_plot_2 <- function(output, input, new_dt_ts_tend, valores_ajustados_sazo){
    print(new_dt_ts_tend)
    print(valores_ajustados_sazo)
     if (input$transformation_sazonalidade == "Diferenciação"){
        autoplot(valores_ajustados_sazo)
     }else{
        new_dt_ts_sazo = new_dt_ts_tend - valores_ajustados_sazo
    autoplot(new_dt_ts_sazo)
     }
    
}

