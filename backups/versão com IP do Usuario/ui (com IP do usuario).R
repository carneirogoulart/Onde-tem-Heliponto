library(shiny)
library(rgeolocate)
library(leaflet)
library(ggmap)
library(tidyverse)
library(readxl)
library(stringr)
library(shinyWidgets)
library(writexl)

#Função busyIndicator do pacote ShinySky (https://github.com/AnalytixWare/ShinySky)

busyIndicator <- function(text = "Calculation in progress..",img = "Icones/ajaxloaderq.gif", wait=1000) {
  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
    ))
    ,shiny::div(class="shinysky-busy-indicator",p(text),img(src=img))
    ,shiny::tags$script(sprintf(
      "	setInterval(function(){
  		 	 if ($('html').hasClass('shiny-busy')) {
  		    setTimeout(function() {
  		      if ($('html').hasClass('shiny-busy')) {
  		        $('div.shinysky-busy-indicator').show()
  		      }
  		    }, %d)  		    
  		  } else {
  		    $('div.shinysky-busy-indicator').hide()
  		  }
  		},100)
  		",wait)
    )
  )	
}

ui <- bootstrapPage(

  
  busyIndicator("Aguarde um instante enquanto trabalhamos no mapa :)",wait = 0, img="Icones/Heli Aguarde.gif"),
  
  #Mudando Tema CSS
  theme="custom.css",
  
  #Plotando Mapa
  leafletOutput("mapa", width = "100%", height = "100%"),
  
  #Botão GitHub
  absolutePanel(id = "logo", bottom = 20, left = 10, width = "auto", fixed=TRUE, draggable = FALSE, height = "auto",
                actionButton("git", label = HTML("<strong>GitHub</strong>") ,icon = icon("github"), width = "110px",
                             onclick ="window.open('https://github.com/carneirogoulart/Onde-tem-Heliponto', '_blank')")),
  
  #Botão Baixar Dados
  absolutePanel(id = "logo", bottom = 60, left = 10, width = "auto", fixed=TRUE, draggable = FALSE, height = "auto",
  downloadButton("download_dados", width = "110px",
                 label=HTML("<strong>Download</strong>"))),
  
  
  #Botão de Pesquisa
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 70, left = -13, right = "auto", bottom = "auto",
                width = 330, height = "auto", 
                h1(""),
                
                
                
  # Pegar IP da Máquina
  tags$script(src="http://pv.sohu.com/cityjson?ie=utf-8"),
  tags$script('$( document ).on("shiny:sessioninitialized", function(event) {Shiny.setInputValue("too",returnCitySN["cip"]);});'),         
  
  
  dropdownButton(
    
  #Filtros de Dados
  textInput("localizacao_manual",
            label=HTML("<strong>Insira a Localização Desejada</strong>"),
            value=NULL),
  
  div(style="display:flex",
  checkboxGroupInput("operacao",
                     label=HTML("<strong>Operação</strong>"),
                     choices= NULL),
  
  
  checkboxGroupInput("tipo",
                     label=HTML("<strong>Tipo</strong>"),
                     choices= NULL)),
  
  
  checkboxGroupButtons("superficie",
                        label=HTML("<strong>Superfície</strong>"),
                        choices= ""),
  
  sliderInput("peso",
              label=HTML("<strong>Peso suportado (em toneladas) </strong>"),
              min=0,
              max=10,
              value=0),
  
  div(style="display:flex",
      checkboxGroupInput("diurna",
                         label=HTML("<strong>Operação Diurna</strong>"),
                         choices= NULL),
      
      
      checkboxGroupInput("noturna",
                         label=HTML("<strong>Operação Noturna</strong>"),
                         choices= NULL)),
  
  
  

  # Action Button
  actionButton("update_chart", label="Buscar", width = "100%", icon = icon("search-location")),
  
  
  icon = icon("search-location"), width = "300px"
  
)

)




)