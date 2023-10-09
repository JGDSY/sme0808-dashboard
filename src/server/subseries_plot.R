

render_subseries_plot <- function(output, input, base_i){

    type_ <- input$ethnicity_type_filter_input

    dias <- as.data.frame(table(base_i$data_referencia))

    
    dias2 = tsibble(
        data = as.Date(dias$Var1),
        casos = dias$Freq,
        index = data 
    )

    monthly <- dias2 %>%
    group_by_key() %>%
    index_by(Year_Month = ~ yearmonth(.)) %>%
    summarise(
        soma_casos = sum(casos)
    )


    G = 
    monthly %>% 
    gg_subseries(soma_casos) + 
    labs(
        y = 'Casos', 
        x = 'Tempo (em anos)', 
        title = 'Casos mensais'
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    return(G)


}