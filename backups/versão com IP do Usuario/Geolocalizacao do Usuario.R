library(shiny)
library(rgeolocate)
library(leaflet)
library(ggmap)
library(tidyverse)

ui <- fluidPage(
  # pegar ip da máquina
  tags$script(src="http://pv.sohu.com/cityjson?ie=utf-8"),
  tags$script('$( document ).on("shiny:sessioninitialized", function(event) {Shiny.setInputValue("too",returnCitySN["cip"]);});'),         
  
  # localizacao desejada
  textInput("localizacao_manual",
            label="Insira a Localização Desejada",
            value=NULL),
  
  
  # printar
  actionButton("update_chart", label="Update Chart", width = "100%"),
  leafletOutput("mapa")
     
)

server <- function(input, output,session) {
  
  # preencher localização desejada inicial com a localização da máquina (atraves do IP)
  observeEvent(input$too,{
    
    ipinfo = as.data.frame(ip_info(input$too))
    
    ipinfo$latitude = as.numeric(substr(ipinfo$loc,1,8))
    
    ipinfo$longitude = as.numeric(substr(ipinfo$loc,10,1000))
    
    ipinfo = isolate(ipinfo)
    
    updateSelectInput(session,
                      "localizacao_manual",
                      selected = paste0(ipinfo$city," - ", ipinfo$region))
    
   # após preencher localização, começar a plotar o mapa
    observeEvent(isolate(input$localizacao_manual),{ output$mapa <- renderLeaflet({
      
      # action button
      if(input$update_chart == 0) {
        
        return()
      }
      
      # pegar latitude e longitude da localização desejada
      register_google(key = "INSIRA AQUI SEU API GOOGLE")
      
      localiza = as.data.frame(geocode(isolate(input$localizacao_manual)))
      
      # criando icone
      estou_aqui_icon <- makeIcon(
        iconUrl = "Icones/helicopter-icon.png",
        iconWidth = 60, iconHeight = 60
      )
      
        #plotando mapa
        leaflet() %>% 
          addTiles() %>%
          addMarkers(lng = localiza$lon, lat = localiza$lat, popup = "<p><b>Você está aqui!</b></p><p>Ou próximo :)</p>", icon=estou_aqui_icon) %>% 
          addProviderTiles("Stamen.Terrain") %>%
          setView(lng = localiza$lon, lat = localiza$lat, zoom = 12)
          
          })
      
    
      
    })
  
  })
  
  
}

shinyApp(ui,server)
