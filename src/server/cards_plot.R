
render_srag_notification_card_plot <- function(df_agg){
    fig <- ggplot(df_agg, aes(x = df_agg$DT_NOTIFIC, y = cumsum(df_agg$value))) +
        geom_smooth(method = "lm", formula = y ~ poly(x, 15), se = FALSE)

        fig <- fig + theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()
        ) 
  
        return(fig)
}