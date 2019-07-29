  library(rnaturalearth)#libudunits2-dev
  library(shiny)
  library(shinydashboard)
  library(raster)
  library(ggplot2)
  library(rasterVis)
  library(rgdal)

setwd("~/")

ui <- dashboardPage(
    dashboardHeader(title = "RconRo"),
    dashboardSidebar(selectInput("opt1","Predicciones futuras para el año 2050 o 2070:",c(50,70),70),
                     selectInput("opt2","Modelo seleccionado:", c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO"),"NO"),
                     selectInput("opt3","Escenario de emisiones CO2:",c(26, 45, 60, 85),85),
                     textInput("opt4","País a evaluar:","spain")),
    dashboardBody(
         
        h5("Por favor espere 10 segundos mientras se descargan los datos"),
       h3("A la izquierda, la precipitación anual promedio entre 1960-1990. A la derecha, las proyecciones para el futuro (2050 / 2070) según distintos modelos CMIP5 y escenarios de emisiones de CO2"),
         plotOutput("Mapa"),
      
    h5("Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978. "),
   h5("Data descargada a partir de http://www.worldclim.org"),
     tags$b("Creador de la app: Rodrigo Díaz Lupanow"),
   tags$b("(programandoconro@gmail.com)")
   ))

server <- function(input, output) { 
   
output$Mapa<-    renderPlot({
    
    esp = ne_countries(country = input$opt4)
    spain_prec_act = getData(name = "worldclim", var = "prec", res = 10, lon= -4, lat=40)
    spain_prec_2050_best = getData(name = "CMIP5", model= input$opt2, rcp= input$opt3, year= input$opt1,res=10, lon= -4, lat=40,var="prec")
    
    r1 <- crop(spain_prec_act, esp)
    r2 <- crop(spain_prec_2050_best, esp)
    
    r1 <- r1[[1]]+r1[[2]]+r1[[3]]+r1[[4]]+r1[[5]]+r1[[6]]+r1[[7]]+r1[[8]]+r1[[9]]+r1[[10]]+r1[[11]]+r1[[12]]
    r2 <- r2[[1]]+r2[[2]]+r2[[3]]+r2[[4]]+r2[[5]]+r2[[6]]+r2[[7]]+r2[[8]]+r2[[9]]+r2[[10]]+r2[[11]]+r2[[12]]
    
    s <-stack(r1,r2)
    
    theme_set(theme_bw())
    gplot(s) + geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
        scale_fill_gradient(low = 'white', high = 'blue',name="An. PPT. (mm)") + 
        coord_equal()+
        theme(strip.text.x = element_blank())+xlab("Long (GD)")+ylab("Lat (GD)")
    
})
    
    }

shinyApp(ui, server)

