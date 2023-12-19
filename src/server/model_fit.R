

get_train_timeseries_split <- function(output, input, decomposed_data){
    residual_data <- decomposed_data()
    train_series = residual_data[1:(length(residual_data) - input$test_steps_slider)]
    return(train_series)
}

get_test_timeseries_split <- function(output, input, decomposed_data){
    residual_data <- decomposed_data()
    test_series = residual_data[(length(residual_data) - input$test_steps_slider):length(residual_data)]
    return(test_series)
}

model_fit <- function(output, input, train_series){
    if(input$model_choice == "Auto ARIMA (Seleção Automatica)"){
        fit <- auto.arima(train_series)
    }else if(input$model_choice == "ARIMA"){
        fit <- arima(train_series, order=c(input$ARIMA_p,input$ARIMA_i,input$ARIMA_q), method="ML")
    }else if(input$model_choice == "AR"){
        fit <- arima(train_series, order=c(input$ARIMA_p,input$ARIMA_i,0), method="ML")
    }else if(input$model_choice == "MA"){
        fit <- arima(train_series, order=c(0,input$ARIMA_i,input$ARIMA_q), method="ML")            
    }else if(input$model_choice == "ARMA"){
        fit <- arima(train_series, order=c(0,input$ARIMA_i,input$ARIMA_q), method="ML")            
    }
    return(fit)
}

get_fit_metrics_table <- function(output, input, adjusted_fit, best_fit, fittedd){
    if(fittedd == T){
        if(input$model_choice == "Auto ARIMA (Seleção Automatica)"){
            a <- data.table(
                Modelo = c("Auto Arima"),
                AIC = c(adjusted_fit$aic),
                loglik = c(adjusted_fit$loglik),
                sigma2 = c(adjusted_fit$sigma2),
                p=c(arimaorder(best_fit)['p'][1]),
                q=c(arimaorder(best_fit)['q'][1]),
                i=c(arimaorder(best_fit)['d'][1])
            )
            return(a)
        }else{
            a <- data.table(
                Modelo = c("Ajustado","Auto Arima", "Diferença"),
                AIC = c(adjusted_fit$aic, best_fit$aic, adjusted_fit$aic-best_fit$aic),
                loglik = c(adjusted_fit$loglik, best_fit$loglik, adjusted_fit$loglik-best_fit$loglik),
                sigma2 = c(adjusted_fit$sigma2, best_fit$sigma2, adjusted_fit$sigma2-best_fit$sigma2),
                p=c(input$ARIMA_p, arimaorder(best_fit)['p'][1], input$ARIMA_p-arimaorder(best_fit)['p'][1]),
                q=c(input$ARIMA_q, arimaorder(best_fit)['q'][1], input$ARIMA_q-arimaorder(best_fit)['q'][1]),
                i=c(input$ARIMA_i, arimaorder(best_fit)['d'][1], input$ARIMA_i-arimaorder(best_fit)['d'][1])
            )
            return(a)
        }

        
    }
}


get_residual_acf_plot  <- function(output, input, adjusted_fit, train_series){
    simulated_data_adjusted <- as.vector(simulate(adjusted_fit, nsim = length(train_series)))
    simulated_acf_adjusted <- acf(simulated_data_adjusted, lag.max = 30, plot = F)

    autoplot(simulated_acf_adjusted)
}


get_residual_hypothesis_testing  <- function(output, input, adjusted_fit){
    pp = checkresiduals(adjusted_fit, plot=F)
    names(pp)

    if(pp$p.value < 0.05){
        runjs('document.getElementById("residual_box").style.backgroundColor = "#f44336";')
        return(paste0("O teste de Ljung-Box REJEITA a hipótese que os residuos são independentes, com valor-p = ", round(pp$p.value, 3)))
    }else{
        runjs('document.getElementById("residual_box").style.backgroundColor = "#04AA6D";')
        return(paste0("O teste de Ljung-Box NÃO REJEITA a independência dos residuos, com valor-p = ", round(pp$p.value, 3)))
    }
        

}


get_forecast_data <- function(output, input, adjusted_fit, valores_ajustados_tendencia, valores_ajustados_sazo, train_series){
    prediction_steps_slider = 2
    forecast_data = forecast(adjusted_fit, h=(input$test_steps_slider + prediction_steps_slider))

    
    forecast_data$x = train_series


    

    if(input$transformation_tendency2 != "Não Aplicar"){
        ff =  na.omit(valores_ajustados_tendencia)
        transformation_part = ff[(length(ff)-length(forecast_data$mean)+1): length(ff)]
        transformation_part2 = ff[1: (length(ff) - length(forecast_data$mean))]
        min_transformation = min(ff)
        forecast_data$mean = forecast_data$mean + transformation_part 
        forecast_data$lower = forecast_data$lower + transformation_part
        forecast_data$upper = forecast_data$upper + transformation_part
        #forecast_data$x + transformation_part2
    }

    if(input$transformation_sazonalidade != "Não Aplicar"){
        ff = na.omit(valores_ajustados_sazo)
        transformation_part = ff[(length(ff)-length(forecast_data$mean)+1): length(ff)]
        min_transformation = min(ff)

        forecast_data$mean = forecast_data$mean + transformation_part
        forecast_data$lower = forecast_data$lower + transformation_part 
        forecast_data$upper = forecast_data$upper + transformation_part

        # forecast_data$x = forecast_data$x + transformation_part2 
    }
    
    

    return(forecast_data)

}

get_inverse_transformed_plot <- function(output, input, adjusted_fit, valores_ajustados_tendencia, valores_ajustados_sazo, test_series, decomposed_data, train_series){
    S = get_forecast_data(output, input, adjusted_fit, valores_ajustados_tendencia, valores_ajustados_sazo, train_series)

    test_series = ts(test_series,start = c(length(decomposed_data)-input$test_steps_slider),frequency = 1)

    # if(input$transformation_tendency2 != "Não Aplicar"){
    #     ff = valores_ajustados_tendencia  %>% replace(is.na(.), 0)
    #     transformation_part = ff[(length(ff)-length(test_series)+1): length(ff)]
    #     min_transformation = min(ff)
    #     test_series = test_series + transformation_part - min_transformation
    # }

    # if(input$transformation_sazonalidade != "Não Aplicar"){
    #     ff = valores_ajustados_sazo  %>% replace(is.na(.), 0)
    #     transformation_part = ff[(length(ff)-length(test_series)+1): length(ff)]
    #     min_transformation = min(ff)
    #     test_series = test_series + transformation_part - min_transformation
    # }
    rmses <- c()
    valsaux <- c()
    for (i in c(1:input$test_steps_slider)){
        rmse_at_i <- sqrt((sum((test_series[1:i] - S$mean[1:i])^2)/i))
        rmses <- c(rmses, rmse_at_i)
        valsaux <- c(valsaux, i)
    }
    
    
    output$forecast_metrics_ <- renderTable({
        metrics <- data.table(
        
            valsaux = valsaux,
            rmse = rmses
        )
        colnames(metrics) <- c("Tamanho do Horizonte de Predição (i)", "RMSE@i")
        metrics
    })

    fig <- autoplot(S)+ autolayer(S, series="Previsão", showgap=F) + autolayer(test_series, series="Passos de Teste")
    return(fig)

}