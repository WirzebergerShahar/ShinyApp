library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data1 <- read.csv("C:/Users/ξεθι/Documents/rshiny/shiny data.csv")

data1$lon<-as.numeric(data1$lon)
data1$lat<-as.numeric(data1$lat)


data3<-data1%>%
  group_by(Country)%>%
  summarise(
    sum_invests=sum(Sum.of.Invests),
    mean_invests = sum(Sum.of.Invests)/n(),
    lon=min(lon),
    lat=min(lat)
  )

data2<-data1 %>%
  group_by(Country,endY)%>%
  summarise(
  sum_invests=sum(Sum.of.Invests),
  mean_invests = sum(Sum.of.Invests)/n(),
  succeededP = sum(Amount.for.success==0),
  n=n(),
  successRate=sum(Amount.for.success==0)/n(),
  lon=min(lon),
  lat=min(lat)
)

data2$moreHalf<-ifelse(data2$successRate>0.5,'High','Low')


ui <- fluidPage(
  fluidRow( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap",width = "100%",height =900), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
absolutePanel(style = "background: white",top = 60, left = 20,height = 200,
                  checkboxInput("all", "total invests", FALSE),
                  checkboxInput("markers","Rate of Succeeded project", FALSE),
                  checkboxInput("heat","active countries", FALSE),
                  sliderInput("year",label = "Choose Year",
                              min = 2010, max = 2019, value = 2015
                  ),
                  plotOutput("distPlot"),draggable = TRUE
    )
  ))


server <- function(input, output, session) {
  #define the color pallate for thesum of invests
  pal <- colorNumeric(
    palette = c('yellow','orange', 'red','brown', 'dark red'),
    domain =data2$sum_invests)
  
  #define the color of for the succeeded rate
  pal2 <- colorFactor(
    palette = c('blue', 'red'),
    domain = data2$moreHalf
  )
  
  #create the map
  output$mymap <- renderLeaflet({
    if(!input$all){
    leaflet(data2) %>% 
      #setView(lng = -99, lat = 45, zoom = 4)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = data2[data2$endY==input$year,], lat = ~ lat, lng = ~ lon, weight = 1, radius = ~sum_invests*0.5, popup = ~as.character(sum_invests), label = ~as.character(paste0("sum_invests: ", sep = " ", sum_invests)), color = ~pal(sum_invests), fillOpacity = 0.5)
 }
    else{
      leaflet(data3) %>% 
        setView(lng = -99, lat = 45, zoom = 4)  %>% #setting the view over ~ center of North America
        addTiles() %>% 
        addCircles(data = data3, lat = ~ lat, lng = ~ lon, weight = 1, radius = ~sum_invests*0.2, popup = ~as.character(sum_invests), label = ~as.character(paste0("sum_invests: ", sep = " ", sum_invests)), color = ~pal(sum_invests), fillOpacity = 0.5)
      
    }})
  
   observe({
    proxy <- leafletProxy("mymap", data = data2[data2$endY==input$year,])
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% clearMarkers() %>% clearControls()
      proxy %>%  addCircleMarkers(stroke = FALSE, color = ~pal2(moreHalf), fillOpacity = 1,label = ~as.character(paste0("succeeded rate: ", sep = " ", moreHalf))) %>%
        addLegend("topright", pal = pal2, values = data2$moreHalf,
                  title = "succeededP Type",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data2[data2$endY==input$year,])
    proxy %>% clearMarkers()
    if (input$heat) {
      proxy %>% clearHeatmap()
      proxy %>%  addHeatmap(lng=~lon, lat=~lat,intensity =~mean_invests , blur =  10, radius = 8) 
    }
    else{
      proxy %>% clearHeatmap()
    }
    
    
  })
  
}

shinyApp(ui, server)
