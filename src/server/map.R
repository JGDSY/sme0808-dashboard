

render_map <- function(output, input, mapa){



    
    br_map <- leaflet(mapa) %>%
        addTiles() %>%
        addPolygons(
            data=mapa,
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.1,
            layerId = ~SIGLA_UF,
            highlightOptions = highlightOptions(color = "green", weight = 2, bringToFront = F, opacity = 1)
        ) 
                            

    return(br_map)
    
}