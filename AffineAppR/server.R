#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  new_img <- reactive({
    req(input$file1)
    image_read(input$file1$datapath)})
  
  img_info <- reactive({
    new_img() %>%
      image_info()
  })
  
  tmp <- tempfile(fileext = ".jpg")
  
  observe({new_img() %>%
      image_threshold(type = "white", threshold = "50%") %>%
      image_negate() %>%
      image_write(path = tmp)})
  
  
  # pre_img <- reactive({ new_img() %>%
  #       image_read() %>%
  #          image_threshold(type = "white", threshold = "50%") %>%
  #          image_negate() %>%
  #          image_write(path = tmp)
  #  })
  
  
  r <- reactive({
    req(input$file1)
    raster(tmp, xmn = 0, xmx = img_info()$width, ymin = 0, ymx = img_info()$height, crs = CRS("+init=epsg:3857"))
  })
  
  
  
  #tempfile(pattern = "file", tmpdir = tempdir(), fileext = )
  
  # tmp <- tempfile()
  
  # inv_img <- reactive({
  #     image_write(
  #     new_img,
  #     path = tmp,
  #     format = "jpg",
  #     quality = NULL,
  #     depth = 8,
  #     density = NULL,
  #     comment = NULL,
  #     flatten = FALSE
  #     )
  # })
  
  
  #crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  #  r <- reactive({raster(new_img())})
  #crs(r()) <- CRS("+init=epsg:3857")
  
  output$leafmap <- renderLeaflet({
    req(input$file1)
    leaflet() %>%
      addRasterImage(r(), colors = "Greys",opacity = 1) %>%
      addDrawToolbar(targetGroup = "drawnPoly", 
                     rectangleOptions = F, 
                     polylineOptions = F, 
                     markerOptions = F, 
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleOptions=F,
                     polygonOptions=drawPolygonOptions(showArea=TRUE, 
                                                       repeatMode=F, 
                                                       shapeOptions=drawShapeOptions(clickable = TRUE)))
  })
  
  ###############
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  value<-reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
  
  #fix the polygon to start another
  
  observeEvent(input$leafmap_draw_new_feature, {
    
    coor<-unlist(input$leafmap_draw_new_feature$geometry$coordinates)
    
    Longitude<-coor[seq(1,length(coor), 2)] 
    
    Latitude<-coor[seq(2,length(coor), 2)]
    
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys<-Polygons(list(poly),    ID=input$leafmap_draw_new_feature$properties$`_leaflet_id`)
    spPolys<-SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                    data=data.frame(notes=NA, row.names=
                                                                                      row.names(spPolys))))
  
    ###plot upon ending draw
    observeEvent(input$leafmap_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('leafmap') %>%  removeDrawToolbar(clearFeatures=TRUE) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% addPolygons(data=value$drawnPoly, popup="poly",   group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>% 
        
        addDrawToolbar(targetGroup = "drawnPoly", 
                       rectangleOptions = F, 
                       polylineOptions = F, 
                       markerOptions = F, 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                       circleOptions=F,
                       polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
      
    })
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
    
  })
  
  observeEvent(input$leafmap_draw_edited_features, {
    
    f <- input$leafmap_draw_edited_features
    
    coordy<-lapply(f$features, function(x){unlist(x$geometry$coordinates)})
    
    Longitudes<-lapply(coordy, function(coor) {coor[seq(1,length(coor), 2)] })
    
    Latitudes<-lapply(coordy, function(coor) { coor[seq(2,length(coor), 2)] })
    
    polys<-list()
    for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
      list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
    )}
    
    spPolys<-SpatialPolygons(polys)
    
    
    SPDF<-SpatialPolygonsDataFrame(spPolys, 
                                   data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
    value$drawnPoly<-rbind(value$drawnPoly, SPDF)
    
  })
  
  observeEvent(input$leafmap_draw_deleted_features, { 
    
    f <- input$leafmap_draw_deleted_features
    
    ids<-lapply(f$features, function(x){unlist(x$properties$layerId)})
    
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,]
    
  }) 
    
  #########################
  # Start of Drawing
  observeEvent(input$leafmap_draw_start, {
    print("Start of drawing")
    print(input$leafmap_draw_start)
  })
  
  # Stop of Drawing
  observeEvent(input$leafmap_draw_stop, {
    print("Stopped drawing")
    print(input$leafmap_draw_stop)
  })
  
  # New Feature
  observeEvent(input$leafmap_draw_new_feature, {
    print("New Feature")
    print(input$leafmap_draw_new_feature)
  })
  
  # Edited Features
  observeEvent(input$leafmap_draw_edited_features, {
    print("Edited Features")
    print(input$leafmap_draw_edited_features)
  })
  
  # Deleted features
  observeEvent(input$leafmap_draw_deleted_features, {
    print("Deleted Features")
    print(input$leafmap_draw_deleted_features)
  })
  
  # We also listen for draw_all_features which is called anytime
  # features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features, {
    print("All Features")
    print(input$leafmap_draw_all_features)
  })
})