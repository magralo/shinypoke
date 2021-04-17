# 
# ## Load comp vision model
# 
# library(reticulate)
# if (Sys.info()['user']=='mateograciano'){
#   virtualenv_create(envname = "python_environment") 
# }else{
#   virtualenv_create(envname = "python_environment", python= "python3") 
# }
# 
# virtualenv_install("python_environment", packages = c('keras', 'pandas','numpy','scipy','scikit-learn', 'tensorflow-cpu','pillow'))
# reticulate::use_virtualenv("python_environment", required = TRUE)
# 
# 
# 
# 
# 
# source_python("tf/pokepred.py")


Sys.setenv("GCS_DEFAULT_BUCKET" = "poke-images",
           "GCS_AUTH_FILE" = "auth.json",
           "GCS_PROJ" = "gentle-analyst-307101")

library(googleCloudStorageR)
library(shiny)
library(tidyverse)
library(plotly)
library(factoextra)
library(janitor)
library(DT)
library(ggdark)
library(leaflet)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(shinyalert)


# Mirror the rainbow, so we cycle back and forth smoothly
colors <- c(normal = "#BABAAE",fighting = "#A75543",flying = "#78A2FF",
            poison ="#A95CA0",ground ="#EECC55",rock = "#CCBD72",bug ="#C2D21E",
            ghost ="#7975D7",steel ="#C4C2DB",fire ="#FA5643",water ="#56ADFF",grass ="#8CD750",
            electric ="#FDE139",psychic ="#FA65B4",ice ="#96F1FF",dragon ="#8673FF",
            dark ="#8D6855",fairy ="#F9AEFF")



make_card_type <- function(title = 'Type',type = 'Grass',id='type1card'){
  
  
  div(class="typecard", id=id,
      div(class="desctype", type)
  )
  
}

poke_data <- read.csv('data/all_stats.csv')

poke_names <- poke_data%>%
  select(name)%>%
  pull()%>%
  unique()