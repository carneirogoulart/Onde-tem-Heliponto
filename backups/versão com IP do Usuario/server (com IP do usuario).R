library(shiny)
library(rgeolocate)
library(leaflet)
library(ggmap)
library(tidyverse)
library(readxl)
library(stringr)
library(shinyWidgets)
library(writexl)

server <- function(input, output,session) {

  
  # dados de heliponto ANAC
  
  heliponto = read_excel("data/Helipontos.xlsx", skip=1)
  
  heliponto = heliponto[c(1,3,5,6,7,8,11,12,13,15,16,17,18)]
  
  colnames(heliponto)[3]="Tipo de Uso e UF"
  
  heliponto$Tipo1="Público"
  
  heliponto[which(is.na(str_extract(heliponto$`Tipo de Uso e UF`, pattern = "Privado"))== FALSE),14]="Privado"
  
  heliponto = heliponto[,-3]
  
  heliponto[which(is.na(heliponto$`Operação Noturna`)),8]="Sem Operação"
  
  heliponto[which(heliponto$`Resistência do Pavimento`== 5000),11]=5
  
  heliponto[which(heliponto$`Operação Noturna`=="IFR"),8]="IFR / VFR"
  
  #Permitir Download de Dados
  output$download_dados = downloadHandler(
    filename = "Base de Dados.xlsx",
    content = function(file){
      
      heliponto %>%
        write_xlsx(file)
      
    }
  )
  
  
  
# Interdepêndencia entre os filtros de dados
  observeEvent(input$too,{
    
    # Dados de IP
    
    ipinfo = as.data.frame(ip_info(input$too))
    
    ipinfo$latitude = as.numeric(substr(ipinfo$loc,1,8))
    
    ipinfo$longitude = as.numeric(substr(ipinfo$loc,10,1000))
    
    ipinfo = isolate(ipinfo)
    
    updateSelectInput(session,
                      "localizacao_manual",
                      selected = paste0(ipinfo$city," - ", ipinfo$region))
    
  
    
    updateCheckboxGroupInput(session,
                        "operacao",
                        choices = unique(heliponto$Tipo1),
                        selected = unique(heliponto$Tipo1))
    
    observeEvent(input$operacao, {
      
      
      elevadosolo = heliponto %>%
        subset(Tipo1 == as.character(input$operacao)) %>%
        select(Tipo) %>%
        unique() 
      
      
      updateCheckboxGroupInput(session,
                               "tipo",
                               choices = elevadosolo$Tipo,
                               selected = elevadosolo$Tipo)
      
      observeEvent(input$tipo,{
        
        sup = heliponto[which(heliponto$Tipo1 %in% input$operacao & heliponto$Tipo %in% input$tipo),]
        
        sup = sup %>%
        select(Superfície) %>%
        unique()
        
        
        updateCheckboxGroupButtons(session,
                                 "superficie",
                                 choices = sup$Superfície,
                                 selected = sup$Superfície)
        
        
        observeEvent(input$superficie,{
          
          
          ton = heliponto[which(heliponto$Tipo1 %in% input$operacao & heliponto$Tipo %in% input$tipo & heliponto$Superfície %in% input$superficie),]
          
          ton = ton %>%
            select(`Resistência do Pavimento`) %>%
            unique()
          
          updateSliderInput(session,
                            "peso",
                            min=min(ton),
                            max=max(ton),
                            value=min(ton))
          
          observeEvent(input$peso,{
            
           diurna = heliponto[which(heliponto$Tipo1 %in% input$operacao & heliponto$Tipo %in% input$tipo & heliponto$Superfície %in% input$superficie & heliponto$`Resistência do Pavimento` >= input$peso),]
            
           diurna = diurna %>%
             select(`Operação Diurna`) %>%
             unique()
           
           updateCheckboxGroupInput(session,
                                    "diurna",
                                    choices = diurna$`Operação Diurna`,
                                    selected = diurna$`Operação Diurna`)
           
           
           observeEvent(input$diurna,{
             
            noturna = heliponto[which(heliponto$Tipo1 %in% input$operacao & heliponto$Tipo %in% input$tipo & heliponto$Superfície %in% input$superficie & heliponto$`Resistência do Pavimento` >= input$peso & heliponto$`Operação Diurna` %in% input$diurna),] 
             
            noturna = noturna %>%
              select(`Operação Noturna`) %>%
              unique()
            
            updateCheckboxGroupInput(session,
                                     "noturna",
                                     choices = noturna$`Operação Noturna`,
                                     selected = noturna$`Operação Noturna`)
             
            
           })
           
          })
          
          
          
        })
        
      })
      
    })
    
    # Plotar Mapa
    observeEvent(isolate(input$localizacao_manual),{ output$mapa <- renderLeaflet({
    
      
      
      # Criar Icone
      estou_aqui_icon = makeIcon(
        iconUrl = "Icones/helicopter-icon.png",
        iconWidth = 60, iconHeight = 60
      )
      
      helipad_icon = makeIcon(
        iconUrl = "Icones/helipad.png",
        iconWidth = 30, iconHeight = 30
      )
      
      
      
      # Action Button -> Plotando Mapa Inicial
      if(input$update_chart == 0) {
        preview = leaflet(options = leafletOptions(minZoom = 4)) %>% 
          addTiles() %>%
          addProviderTiles("Stamen.Terrain")%>%
          setView(lng = -47.9292, lat = -15.7801, zoom = 4) %>%
          addMarkers(lng = heliponto$LonGeoPoint, lat = heliponto$LatGeoPoint,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 40),
                     icon=helipad_icon,
                     popup = paste0("<strong>Nome: </strong>",heliponto$Nome," (",heliponto$`Código OACI`,")",
                                    "<br><strong>Tipo: </strong>", heliponto$Tipo,
                                    "<br><strong>Operação: </strong>", heliponto$Tipo1,
                                    "<br><strong>Altitude: </strong>", heliponto$Altitude, " m",
                                    "<br><strong>Superfície: </strong>", heliponto$`Formato da Área de Pouso`, " de ",
                                    heliponto$Superfície, " (",heliponto$Dimensões, ") ",
                                    "<br><strong>Suporta até: </strong>", heliponto$`Resistência do Pavimento`, " toneladas",
                                    "<br><strong>Operação Diurna: </strong>", heliponto$`Operação Diurna`,
                                    "<br><strong>Operação Noturna: </strong>", heliponto$`Operação Noturna`)
                     )
          
        return(preview)
        
      }
      
      # Latitude e Longitude da Localização desejada do API Google
      register_google(key = "INSIRA AQUI SEU API GOOGLE")
      
      localiza = as.data.frame(geocode(isolate(input$localizacao_manual)))
      

      
      heliponto_selecao = heliponto[which(heliponto$Tipo1 %in% isolate(input$operacao) & heliponto$Tipo %in% isolate(input$tipo) & heliponto$Superfície %in% isolate(input$superficie) & heliponto$`Resistência do Pavimento` >= isolate(input$peso) & heliponto$`Operação Diurna` %in% isolate(input$diurna) & heliponto$`Operação Noturna` %in% isolate(input$noturna)),] 
      
      
      # Plotando Mapa, se não houver localização desejada
      if(is.na(localiza$lon) == TRUE) {
        
        semlocaliza = leaflet(options = leafletOptions(minZoom = 4)) %>%
          addTiles() %>%
          addProviderTiles("Stamen.Terrain") %>%
          setView(lng = -47.9292, lat = -15.7801, zoom = 4) %>%
          addMarkers(lng = heliponto_selecao$LonGeoPoint, lat = heliponto_selecao$LatGeoPoint,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 40),
                     icon=helipad_icon,
                     popup = paste0("<strong>Nome: </strong>",heliponto_selecao$Nome," (",heliponto_selecao$`Código OACI`,")",
                                    "<br><strong>Tipo: </strong>", heliponto_selecao$Tipo,
                                    "<br><strong>Operação: </strong>", heliponto_selecao$Tipo1,
                                    "<br><strong>Altitude: </strong>", heliponto_selecao$Altitude, " m",
                                    "<br><strong>Superfície: </strong>", heliponto_selecao$`Formato da Área de Pouso`, " de ",
                                    heliponto_selecao$Superfície, " (",heliponto_selecao$Dimensões, ") ",
                                    "<br><strong>Suporta até: </strong>", heliponto_selecao$`Resistência do Pavimento`, " toneladas",
                                    "<br><strong>Operação Diurna: </strong>", heliponto_selecao$`Operação Diurna`,
                                    "<br><strong>Operação Noturna: </strong>", heliponto_selecao$`Operação Noturna`)
          )
        
        return(semlocaliza)
      }
      
      # Plotando Mapa, se houver localização desejada
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        addTiles() %>%
        addMarkers(lng = localiza$lon, lat = localiza$lat, popup = "<b>Você está aqui!</b>", icon=estou_aqui_icon) %>% 
        addProviderTiles("Stamen.Terrain") %>%
        setView(lng = localiza$lon, lat = localiza$lat, zoom = 12) %>%
        addMarkers(lng = heliponto_selecao$LonGeoPoint, lat = heliponto_selecao$LatGeoPoint,
                   clusterOptions = markerClusterOptions(maxClusterRadius = 40),
                   icon=helipad_icon,
                   popup = paste0("<strong>Nome: </strong>",heliponto_selecao$Nome," (",heliponto_selecao$`Código OACI`,")",
                                  "<br><strong>Tipo: </strong>", heliponto_selecao$Tipo,
                                  "<br><strong>Operação: </strong>", heliponto_selecao$Tipo1,
                                  "<br><strong>Altitude: </strong>", heliponto_selecao$Altitude, " m",
                                  "<br><strong>Superfície: </strong>", heliponto_selecao$`Formato da Área de Pouso`, " de ",
                                  heliponto_selecao$Superfície, " (",heliponto_selecao$Dimensões, ") ",
                                  "<br><strong>Suporta até: </strong>", heliponto_selecao$`Resistência do Pavimento`, " toneladas",
                                  "<br><strong>Operação Diurna: </strong>", heliponto_selecao$`Operação Diurna`,
                                  "<br><strong>Operação Noturna: </strong>", heliponto_selecao$`Operação Noturna`)
        )
     
      
      
      
      
    })
    
    
    
    })
    
  })
  
  
}
