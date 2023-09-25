

render_map <- function(output, input){
    mapa  <- readOGR("./resources/", "BR_UF_2022", stringsAsFactors=FALSE, encoding="UTF-8")

    
    br_map <- leaflet(mapa) %>%
        addTiles() %>%
        addPolygons(
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.1
        )

    return(br_map)
    
}