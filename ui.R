

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)



##https://www.youtube.com/watch?v=wXjSaZb67n8&ab_channel=JustinKim

pokedex_base <- function(...){
    
    div( class="pokedex",
         div( class="left-container",
              div( class="left-container__top-section",
                   div( class="top-section__blue"),
                   div( class="top-section__small-buttons",
                        div(class="top-section__red",onclick="upred(this)"),
                        div(class="top-section__yellow",onclick="upyellow(this)"),
                        div(class="top-section__green",onclick="upgreen(this)"),
                        div(class="top-section__black",onclick="upblack(this)")
                   )
              ),
              div(class="left-container__main-section-container",
                  div(class="left-container__main-section",
                      div( class="main-section__white",
                           div( class="main-section__black",
                                ...
                           ),
                           
                      )
                  )
              )
         )
    )
}


poke_names <- read.csv('data/all_stats.csv')%>%
    select(name)%>%
    pull()%>%
    unique()



ui <- fillPage(
    includeCSS("www/style.css"),
    includeScript(path = "www/app2.js"),
    pokedex_base(
        fluidPage(
            selectInput('sel1','Select your pokemon',choices = poke_names),
            uiOutput('poke_info_1')
        )
    )
)





           



