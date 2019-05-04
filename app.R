#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(rtiff)
library(tiff)
library(raster)
library(rgdal)#hacia falta la libreria sudo apt-get install libgdal1-dev libproj-dev libgdal-dev
library(sp)
library(maptools)
library(vetools)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(viridis)
library(leaflet)
setwd("~/Documents/future climate/mapas10min")

# get all the tif files
m <- list.files(path = "/home/ro/Documents/future climate/mapas10min", pattern = "*.tif", full.names = F)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Clima del futuro en distintas zonas del planeta"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("Loi", "Longitud E izquierda", -73, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("Lod", "Longitud E derecha", -64, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("Laar", "Latitud N inferior", 5, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("Laab", "Latitud N superior", 11, min = NA, max = NA, step = NA,
                   width = NULL),
      h3("Escenario 1"),
      selectizeInput("options5","Variable",choices = c("pr","tn","tx")),
      selectizeInput("options","Modelo",choices = c("bc","cc","gs","hd","ip","mc","mg","no")),
      selectizeInput("options2","Escenario",choices = c("26","45","60","85")),
      selectizeInput("options3","Año",choices = c("50","70")),
      selectizeInput("options4","Mes",choices = c("01","02","03","04","05","06","07","08","09","10","11","12")),
      h3("Escenario 2"),
      selectizeInput("options5b","Variable",choices = c("pr","tn","tx")),
      selectizeInput("optionsb","Modelo",choices = c("bc","cc","gs","hd","ip","mc","mg","no")),
      selectizeInput("options2b","Escenario",choices = c("26","45","60","85")),
      selectizeInput("options3b","Año",choices = c("50","70")),
      selectizeInput("options4b","Mes",choices = c("01","02","03","04","05","06","07","08","09","10","11","12")),
      h3("Escenario actual (año 2000"),
      selectizeInput("options5c","Variable",choices = c("prec","tmin","tmax")),
      selectizeInput("options4c","Mes",choices = c("01","02","03","04","05","06","07","08","09","10","11","12"))
    ),
    
    # Show 2 plots for comparison
    mainPanel(
     leafletOutput("distPlot")
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$distPlot <- renderLeaflet({
    
    su<- list.files(path = "/home/ro/Documents/future climate/mapas10min", 
                    pattern = input$options5)
    
        uy<- subset(su,grepl(input$options4,su,fixed = F))
        uy2<- subset(uy,grepl(input$options,uy,fixed = F))
      uy3<- subset(uy2,grepl(input$options3,uy2,fixed = F))
        uy4<- subset(uy3,grepl(input$options2,uy3,fixed = F))
 
    su2<- list.files(path = "/home/ro/Documents/future climate/mapas10min", 
                     pattern = input$options5b)
  
      uyb<- subset(su2,grepl(input$options4b,su2), drop = TRUE)
    uy2b<- subset(uyb,grepl(input$optionsb,uyb), drop = TRUE)
    uy3b<- subset(uy2b,grepl(input$options3b,uy2b), drop = TRUE)
    uy4b<- subset(uy3b,grepl(input$options2b,uy3b), drop = TRUE)
    
    
    suc<- list.files(path = "/home/ro/Documents/future climate/mapas10min", 
                     pattern =  "wc")
    
    uyc<- subset(suc,grepl(input$options5c,suc,fixed = F))
    
    uyc2<- subset(uyc,grepl(input$options4c,uyc,fixed = F))
    
     setwd("~/Documents/future climate/mapas10min")
     l3<-raster(uy4[1])
    l2b<-raster(uy4b[1])
    l2<-raster(uyc2[1])
    
     mi <- extent(input$Loi, input$Lod, input$Laar, input$Laab)#demarcar el área geográfica de interes 
    
    sb=crop(l2b,mi)
    s=crop(l3,mi) 
    sc=crop(l2,mi) 

  ss<-stack(s,sb,sc)
  
  pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(s),
                      na.color = "transparent") 
   
  leaflet() %>% addTiles() %>%
    addRasterImage(s, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal, values = values(s),
              title = "variable")

  })

}



# Run the application 
shinyApp(ui = ui, server = server)

