
get_data_to_lag_plot <- function(df){

    print(df)
    df$DT_NOTIFIC <- as.Date(df$DT_NOTIFIC, format="%d/%m/%Y")
    data <- df %>% group_by(DT_NOTIFIC) %>% summarise(frequency = n())

    tsdata = data %>% as_tsibble()
    print(tsdata)
    tsdata <- tsdata %>% fill_gaps(.full = TRUE)

    return(tsdata)
}

get_data_to_lag_plot2 <- function(tsdata){

    monthly <- tsdata %>%
    group_by_key() %>%
    index_by(month = ~ yearmonth(.)) %>%
    summarise(
        casos = sum(frequency)
    )
    return(monthly)
}


render_lag_plot <- function(output, input, tsdata){


    G0 = tsdata %>% 
    autoplot() +
    labs(
        x = 'Data',
        y = 'Notificações',
         title="Serie Temporal"
    ) +
    theme_minimal()




    
    return(ggplotly(G0))


}

render_lag_plot2 <- function(output, input, tsdata){

    G1 = 
    tsdata %>% 
    ACF() %>% 
    autoplot() +
    labs(
        x = 'Defasagem',
        y = 'Autocorrelação',
         title="Grafico de Autocorrelação (Lag 31D)"
    ) +
    coord_cartesian(ylim=c(-1,1)) +
    theme_minimal()


    
    return(ggplotly(G1))

}

render_lag_plot3 <- function(output, input, monthly){


   G0 = 
  monthly %>% 
  autoplot() +
  labs(
    x = 'Data',
    y = 'Notificações',
    title="Serie Temporal"
  ) +
  theme_minimal()

    return(ggplotly(G0))


}


render_lag_plot4 <- function(output, input, monthly){


   G1 = 
    monthly %>% 
    ACF() %>% 
    autoplot() +
    labs(
        x = 'Defasagem',
        y = 'Autocorrelação',
        title="Grafico de Autocorrelação (Lag 16M)"
    ) +
    coord_cartesian(ylim=c(-1,1)) +
    theme_minimal()

}

render_lag_plot_diff <- function(output, input, data){


   G1 = ggAcf(data, lag=31)

}

render_lag_plot_diff2 <- function(output, input, data){


   bbb = pacf(data, pl=F,lag=31)
   G1 = autoplot(bbb)

}