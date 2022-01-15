library(shiny)
library(rgeolocate)
library(leaflet)
library(ggmap)
library(tidyverse)
library(readxl)
library(stringr)
library(shinyWidgets)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}, "),
  
  tags$style("
  
  /* Customize fonts */
body, label, input, button, select { 
  font-family: 'Helvetica Neue', Helvetica;
  font-weight: 200;
}
h1, h2, h3, h4 { font-weight: 400; }

        #controls {
  /* Appearance */
  background-color: #F0F8FF;
  padding: 0 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.8;
  zoom: 0.9;
  transition: opacity 500ms 1s;
}
#controls:hover {
  /* Fade in while hovering */
  opacity: 1;
  transition-delay: 0;
}"),

  
  leafletOutput("mapa", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto", 
                h1(""),
                
                
                
  # pegar ip da máquina
  tags$script(src="http://pv.sohu.com/cityjson?ie=utf-8"),
  tags$script('$( document ).on("shiny:sessioninitialized", function(event) {Shiny.setInputValue("too",returnCitySN["cip"]);});'),         
  
  # localizacao desejada
  
  
  textInput("localizacao_manual",
            label="Insira a Localização Desejada",
            value=NULL),
  
  div(style="display:flex",
  checkboxGroupInput("operacao",
                     label="Operação",
                     choices= NULL),
  
  
  checkboxGroupInput("tipo",
                     label="Tipo",
                     choices= NULL)),
  
  
  checkboxGroupButtons("superficie",
                        label="Superficie",
                        choices= ""),
  
  sliderInput("peso",
              label="Peso (em toneladas) máximo suportado",
              min=0,
              max=10,
              value=0),
  
  div(style="display:flex",
      checkboxGroupInput("diurna",
                         label="Operação Diurna",
                         choices= NULL),
      
      
      checkboxGroupInput("noturna",
                         label="Operação Noturna",
                         choices= NULL)),
  
  
  

  # printar
  actionButton("update_chart", label="Buscar", width = "100%", icon = icon("search-location")),
  
  
)

)