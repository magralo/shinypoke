

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinyalert)



##https://www.youtube.com/watch?v=wXjSaZb67n8&ab_channel=JustinKim
#https://github.com/angle943/pokedex

pokedex_base <- function(...){
    
    div( class="pokedex",
         div( class="left-container",
              div( class="left-container__top-section",
                   div( class="top-section__blue"),
                   div( class="top-section__small-buttons",
                        div(class="top-section__red"),
                        div(class="top-section__yellow"),
                        div(class="top-section__green")
                   )
              ),
              div(class="left-container__main-section-container",
                  div(class="left-container__main-section",
                      div( class="main-section__white",
                           div( class="main-section__black",
                                ...
                           ),
                           
                      )
                  ),
                  div(class="left-container__controllers",
                      div(class="controllers__d-pad",
                          div(class="d-pad__cell top",onclick="upbut(this)"),
                          div(class="d-pad__cell left",onclick="leftbut(this)"),
                          div(class="d-pad__cell middle"),
                          div(class="d-pad__cell right",onclick="rightbut(this)"),
                          div(class="d-pad__cell bottom",onclick="downbut(this)")
                       ),
                      div(class="controllers__buttons",
                          div(class="buttons__button", 'B',onclick="Bbut(this)"),
                          div(class="buttons__button", 'A',onclick="Abut(this)")
                      )
                      
                  ), 
                  div(class="controllers_page-pad",
                      div(class="lowspace"),
                      div(class="low-container__controllers",
                          div(class="buttons__page", 'Home',onclick="homebut(this)"),
                          div(class="buttons__page", 'Chain',onclick="chainbut(this)"),
                          div(class="buttons__page", 'Clusters',onclick="clusterbut(this)"),
                          div(class="buttons__page", 'Rival',onclick="rivalbut(this)")
                      ),
                      div(class="lowspace")
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
    useShinyalert(), 
    pokedex_base(
        fluidPage(
            selectInput('sel1','Select your pokemon',choices = poke_names),
            
            uiOutput('poke_info_1'),id='allpokeinfo'
        )
    )
)





           



