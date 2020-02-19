


server <- function(input, output) {
  
  df2$colp <- cut(df2$per, 20)
  factpal <- colorFactor(sample(rainbow(20),20), domain = df2$colp)
  
  
  dfp <- reactive({
    d <- subset(df2, per>=input$per_adjust)
    return(d)
  })
  
  sp_fw_proj <- reactive({
    df <- subset(dyadxy, lab == input$lab)
    coords2 <- cbind(df$lon, df$lat)
    sp_fw <- SpatialPointsDataFrame(coords = coords2, data = df, proj4string = CRS("+proj=longlat +datum=WGS84"))
    sp_fw_proj <- spTransform(sp_fw, CRS("+init=epsg:26914"))
    return(sp_fw_proj)
  })
  hull <- reactive({
    j<- gConvexHull(sp_fw_proj())
    return(j)
  })
  
  hull_xy <- reactive({
    k<- spTransform(hull(), CRS("+proj=longlat +datum=WGS84"))
    return(k)
  })

  output$mymap <-  renderLeaflet({
                    data<- subset(dfp(),lab==input$lab)
                    lab2<- subset(labxy, Testing.Lab==input$lab)
                    hull_xy2<- hull_xy()
                    leaflet() %>% addTiles() %>% 
                      addPolygons(data=mwi1, color='blue') %>%
                      addPolygons(data=hull_xy2, color='green', fill='green') %>%
                      addCircleMarkers(data=data, popup=~label, radius=3, color = ~factpal(colp), fillOpacity = 0.5) %>%
                      # addCircleMarkers(clusterOptions = markerClusterOptions(), data=data, popup=~label, radius=3, color = ~factpal(colp), fillOpacity = 0.5) %>%
                      addMarkers(lng=lab2$X, lat=lab2$Y, popup=input$lab)
                      
                  })
  
  output$network <- renderVisNetwork({
    visNetwork(node.lf, edge.lfx) %>% 
      visOptions(highlightNearest =list(enabled=T),
                 selectedBy = list(variable = "Laboratory", multiple = T)) %>%
      visNodes(color=list(border = "darkblue")) %>%
      visLayout(randomSeed = 123) %>%
      visEdges(physics = F) %>%
      #visPhysics(enabled=F) %>%
      visIgraphLayout() %>%
      visInteraction(navigationButtons = TRUE)})
  
 


   hull_data <- reactive({
    d <- subset(dyadxy, per>=input$per_adjust2)
    return(d)
  })
  
  dyadxy_hull<- reactive({
    d1 <- hull_data()
    dyadxy3 = na.omit(d1) %>%
                group_by(lab) %>%
                mutate(hull = 1:n(), hull = factor(hull, chull(lat, lon))) %>%
                arrange(hull)
  return(dyadxy3)
    })
  
  output$hulls <- renderPlot({
    d3 <- dyadxy_hull()
    ggplot(d3, aes(lon, lat, color = lab, fill = lab)) +
      geom_polygon(data = filter(d3, !is.na(hull)), alpha = 0.5) +
      geom_point() +
      guides(color = FALSE, fill = FALSE) +
      theme_bw() +
      theme(axis.text = element_blank())
  })
}

