



render_transformation_plot <- function(output, input, base_i){
    df <- base_i %>%
        count(DT_NOTIFIC)

    df <- df %>%
    filter(year(DT_NOTIFIC) < 2020)

    df_ts <- tsibble(date = df$DT_NOTIFIC, value = df$n, index = date)


    # Plotando as series
    G0 = 
    df_ts %>% 
    autoplot(value) +
    labs(
        y = 'Numero de casos',
        x = 'Tempo (em dias)'
    )

    return(G0)
}



render_transformation_plot2 <- function(output, input, base_i){
    df <- base_i %>%
        count(DT_NOTIFIC)

    df <- df %>%
    filter(year(DT_NOTIFIC) < 2020)

    df_ts <- tsibble(date = df$DT_NOTIFIC, value = df$n, index = date)
    lambda <- df_ts %>% 
        features(df_ts$value, features=guerrero)  %>% 
        pull(lambda_guerrero)
    
    df_ts <- df_ts %>%
        mutate(value_bxcx = box_cox(value, lambda))


    G1 = 
    df_ts %>% 
    autoplot(value_bxcx) +
    labs(
        y = 'Numero de casos (transformada por Box-Cox)',
        x = 'Tempo (em dias)',
        title = (
        paste0('lambda = ',round(lambda,3))
        )
    )
    return(G1)
}