
get_data_menor <- function(base_i){
    menor_data = min(base_i$data_referencia)
    data_menor = c(as.numeric(format(menor_data,'%Y')),
               as.numeric(format(menor_data,'%m')),
               as.numeric(format(menor_data,'%d')))
    return(data_menor)
}


dataset_after_variance_transformation_base <- function(output, input, df){

    freq = 365

    dt = df[,.(total = .N),by = data_referencia]
    dt = dt[order(data_referencia)]
    

    return(dt)

    

}

dataset_after_variance_transformation1 <- function(output, input, dt, data_menor){

    freq = 365

    dt_ts = ts(dt$total,start = c(data_menor[1],data_menor[2],data_menor[3]),frequency = freq)

    return(dt_ts)

    

}

dataset_after_variance_transformation2 <- function(output, input, dt, data_menor){

    freq = 365


    total = dt$total

    
    if(input$transformation_variance == "Não Aplicar"){
        total_transformado = dt$total
    }else if(input$transformation_variance == "Box-Cox"){
        lambda = BoxCox.lambda(dt$total)
        total_transformado = BoxCox(dt$total,lambda)
    }else if(input$transformation_variance == "Log"){
        total_transformado = log(dt$total)
    }else{
        total_transformado = sqrt(dt$total)
    }

    dt_i = ts(total_transformado,start = c(data_menor[1],data_menor[2],data_menor[3]),frequency = freq)

    return(dt_i)

}

render_variance_plot <- function(output, input, dt_i, dt_ts){

    
    if(input$transformation_variance == "Não Aplicar"){

        title1 = 'Série Atual'
        ytitle1 = 'Valores atuais'
        plot1 =ggplotly(autoplot(dt_ts)) %>% 
            layout(yaxis = list(title = ytitle1), xaxis=list(title='Data'))

        
        #grafico de sazonalidade serie original
        title3 = 'Série Atual'
        ytitle3 = 'Valores atuais'
        plot3 =ggplotly(ggseasonplot(dt_ts)) %>% 
            layout(yaxis = list(title = ytitle3), xaxis=list(title='Data'))
        
        fig <- subplot(plot1,plot3,nrows = 2,titleX = T,titleY = T,margin = 0.05)
        return(fig)
    }else{

        title1 = 'Série Atual'
        ytitle1 = 'Valores atuais'
        plot1 =ggplotly(autoplot(dt_ts)) %>% 
            layout(yaxis = list(title = ytitle1))  

        
        #grafico da serie pos aplicacao de boxcox
        title2 = 'Série transformada'
        ytitle2 = 'Valores transformados'
        plot2 = ggplotly(autoplot(dt_i)) %>% 
            layout(yaxis = list(title = ytitle2)) 
        
        
        #grafico de sazonalidade serie original
        title3 = 'Série Atual'
        ytitle3 = 'Valores atuais'
        plot3 =ggplotly(ggseasonplot(dt_ts)) %>% 
            layout(yaxis = list(title = ytitle3), xaxis=list(tickangle = 90, title='Data'))
        
        # grafico de sazonalidade serie pos box cox
        title4 = 'Série transformada'
        ytitle4 = 'Valores transformados'
        plot4 = ggplotly(ggseasonplot(dt_i)) %>% 
            layout(yaxis = list(title = ytitle4), xaxis=list(tickangle = 90, title='Data'))
        
        fig <- subplot(plot1,plot2,plot3,plot4,nrows = 2,titleX = T,titleY = T,margin = 0.05)


        return(fig)

    }
    

}

dataset_after_tendency_transformation <- function(output, input, dt_ts){

    if (input$transformation_tendency == "Não Aplicar"){
        return(dt_ts)

    }else if (input$transformation_tendency == "Media Movel"){

        rollavg = input$transformation_rollavg_input

        dt_i = stats::filter(dt_ts, filter = rep(1/rollavg, rollavg), method = 'convolution', sides = 1)
        return(dt_i)

    }else{
        lag_dif = input$transformation_diff_input
        dt_i = diff(dt_ts, lag = lag_dif)
        return(dt_i)

    }
    
}


render_tendency_plot <- function(output, input, dt_i, dt_ts){


    title1 = 'Série Atual'
    ytitle1 = 'Valores atuais'
    plot1 =ggplotly(autoplot(dt_ts)) %>% 
        layout(yaxis = list(title = ytitle1))
    

    title2 = 'Série Diferenciada'
    ytitle2 = 'Valores diferenciados'
    plot2 = ggplotly(autoplot(dt_i)) %>% 
        layout(yaxis = list(title = ytitle2))
    
    return(subplot(plot1,plot2,titleX = T,titleY = T,nrows = 1,margin = 0.05))
    

}

get_decomposition <- function(output, input, dt_ts){
    if (input$transformation_decomposition == "Não Aplicar"){

        return(dt_ts)

    } else if (input$transformation_decomposition == "Decomposição Aditiva"){
        decomposicao_objeto = decompose(dt_ts)

        return(decomposicao_objeto$random)

    } else {
         decomposicao_objeto = decompose(dt_ts, type="multiplicative")

        return(decomposicao_objeto$random)

    }
} 

render_decomposition_plot <- function(output, input, dt_ts){

    if (input$transformation_decomposition == "Não Aplicar"){

        return(ggplotly(autoplot(dt_ts)))

    } else if (input$transformation_decomposition == "Decomposição Aditiva"){
        decomposicao_objeto = decompose(dt_ts)

        return(ggplotly(autoplot(decomposicao_objeto)))

    } else {
         decomposicao_objeto = decompose(dt_ts, type="multiplicative")

        return(ggplotly(autoplot(decomposicao_objeto)))

    }
    
        

}