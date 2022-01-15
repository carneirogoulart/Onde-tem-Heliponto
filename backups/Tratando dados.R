library(tidyverse)
library(readxl)
library(stringr)

heliponto = read_excel("data/Helipontos.xlsX", skip=1)

heliponto = heliponto[c(1,3,5,6,7,8,11,12,13,15,16,17,18)]

colnames(heliponto)[3]="Tipo de Uso e UF"

heliponto$Tipo="na"

heliponto[which(is.na(str_extract(heliponto$`Tipo de Uso e UF`, pattern = "Privado"))== FALSE),4]="Privado"

heliponto[which(is.na(str_extract(heliponto$`Tipo de Uso e UF`, pattern = "Público"))== FALSE),4]="Público"

heliponto = heliponto[,-3]
