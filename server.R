#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    rv <- reactiveValues() ### Auxiliar for all plots of a given pokemon
    
    
    ## The only plot that is not made with this reactivity is the kmeans plot
    
    observe({
        #### DELETE prev data
        rv$pcluster = NULL
        rv$chain_plot = NULL
        rv$compare_plot = NULL
        rv$poke_map = NULL
        ####
        rv$name=input$sel1
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()%>%
            first()
        
        rv$poke_id <-poke_id
        basic_info = get_usual_info(poke_id[1])
        rv$basic_info <- basic_info
        
        color = unname(colors[basic_info$type[1]])
        rv$color = color
        
        color2 = unname(colors[basic_info$type[2]])
        rv$color2 = color2
        
        
        type = basic_info$type[1]
        rv$type = type
        
        type2 = basic_info$type[2]
        rv$type2 = type2
        
        poke_img_str = poke_data%>%
            filter(name==input$sel1)%>%
            select(im1)%>%
            pull()%>%
            first()
        
        rv$poke_img_str = poke_img_str

        
        poke_stats <- poke_data%>%
            filter(name==input$sel1)
        
        rv$poke_stats <- poke_stats
        
        
        #### Stats table
        
        
        
        df <- poke_stats%>%
          select(stat,base_stat)%>%
          rename(name=stat,base=base_stat)
        
        rv$stats_table <-DT::datatable(df,rownames = FALSE,
                                       options = list(searching = FALSE,
                                       paging=FALSE,
                                       info=FALSE,
                                       initComplete = JS("function(settings, json) {",
                                                          "$(this.api().table().body()).css({'font-size': '200%'});"
                                                          ,"$(this.api().table().header()).css({'display': 'none'});",
                                                          "}")))%>%
          formatStyle(
            'base',
            background = styleColorBar(df$base, color),
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'left'
          )%>%
          formatStyle(columns=colnames(df),color=color,background = 'black')
        
        ### update css
        
        session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
        if (color2==color){
            session$sendCustomMessage("hide-type2",color) ## CALL JS FUNCTION    
        }
        session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
        
        
    })
    
    observe({
      
      color = rv$color 
      
      color2 =rv$color2 
      
      session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
      if (color2==color){
        session$sendCustomMessage("hide-type2",color) ## CALL JS FUNCTION    
      }
      session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
      
    })
    

    output$poke_stats <-DT::renderDataTable({
      
      rv$stats_table
      
    })
    
    
    output$poke_head <- renderUI({
      
      
      
    })
    
    output$poke_info_1 <- renderUI({
        
      
      poke_img <- img(src=rv$poke_img_str,width="50%")
      
      
      color = rv$color
      color2 =rv$color2
      
      
    
      type_cards <- fillPage(make_card_type(type = rv$type),
                             make_card_type(title = 'Sec Type',type = rv$type2,id='type2card')
      )
      
      poke_head <- fillPage(
        fluidRow(column(4,poke_img),
                 column(2,type_cards),
                 column(4,DT::dataTableOutput('poke_stats')))
      )
      
      
      
      
      
      if (is.null(input$page_count)){
        page <- 1
      }else{
        page <- input$page_count
      }
      
      if (page==1){
        
        aux <- predUI("pred")
        
      } else if (page==2) {
        
        aux <- chainUI("chain")
        
      }else if (page==3) {
        
        aux <- clusterUI("cluster")
        
      } else if (page==4) {
        
        aux <- fightUI("fight")
      } 
      
      
      
      
      fluidPage(poke_head,aux)
        

        
        
    })

    
    
    ### Stats table
    output$poke_stats <-DT::renderDataTable({
      
      rv$stats_table
      
    })
    
    ### ancho
    get_ancho <- reactive({input$dimension[1]})
    
    
    ### Chain plot 
    get_id <- reactive({rv$poke_id})
    get_color <- reactive({rv$color})
    get_color2 <- reactive({rv$color2})
    ### We must pass the id (pokedex number) and colors for the graphs
    chainServer("chain",poke_id=get_id,color=get_color,color2=get_color2,getancho=get_ancho) 
    
    
    ### Prediction Module
    
    get_data <- reactive({poke_data})
    prediction=predServer("pred",poke_data=get_data,getancho=get_ancho)
    
    observe({### After we get the prediction we update the selected pokemon
     # print(prediction())
      updateVarSelectInput(session,"sel1",selected = prediction())
    })
    
    
    ### Fight module
    
    get_stats1 <- reactive({rv$poke_stats})
    fightServer("fight",poke_data=get_data,get_color,get_stats1,getancho=get_ancho)
    
    observe({### After we get the prediction we update the selected pokemon
      updateVarSelectInput(session,"sel1",selected = prediction())
    })
    
    
    ### cluster module

    clusterServer("cluster",pokemon=get_id,getancho=get_ancho)
    

    
    ### Location/pokemon go
    observeEvent(input$getloc,{
        session$sendCustomMessage("update_location", 'x')
        
    })
    
    
   
    
    ### Solve reactivity issues for the background
    
    timer <- reactiveTimer(500)
    
    
    observe({
        
        timer()

        

        
        
        
        color = rv$color
        color2 = rv$color2
        
        session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
        if (color2==color){
            session$sendCustomMessage("hide-type2",color) ## CALL JS FUNCTION    
        }
        session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
        
        if(!is.null(input$sel2)){
            
            color = rv2$color
            color2 = rv2$color2
            
            session$sendCustomMessage("background-type-rival", color) ## CALL JS FUNCTION
            if (color2==color){
                session$sendCustomMessage("hide-type2-rival",color) ## CALL JS FUNCTION    
            }
            session$sendCustomMessage("background-type2-rival", color2) ## CALL JS FUNCTION
            
        }
        
    })
    
    
    ### Use of up down buttons
    observe({
        
        
        poke_id = poke_data%>%
            filter(name==isolate(input$sel1))%>% ### When the user change the input you dont want to update twice (and forever)
            select(pos)%>%
            pull()
        
        if (!is.null(input$updown_but)){
            print(input$id_count)
            if (input$updown_but=='up'){
                
                poke_id = min(poke_id + 1,max(poke_data$pos))
                
                poke_name <- poke_data%>%
                    filter(pos==poke_id)%>%
                    select(name)%>%
                    pull()%>%
                    first()
                
                updateVarSelectInput(session,"sel1",selected = poke_name)
            }else {
                poke_id = max(poke_id - 1,1)
                
                poke_name <- poke_data%>%
                    filter(pos==poke_id)%>%
                    select(name)%>%
                    pull()%>%
                    first()
                
                updateVarSelectInput(session,"sel1",selected = poke_name)
            }  
        }
        
    })
    
    observe({
      session$sendCustomMessage("playcrie", paste0('cries/',rv$poke_id,'.ogg')) ## CALL JS FUNCTION
    })



})

