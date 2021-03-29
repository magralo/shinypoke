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

# Mirror the rainbow, so we cycle back and forth smoothly
colors <- c(normal = "#BABAAE",fighting = "#A75543",flying = "#78A2FF",
    poison ="#A95CA0",ground ="#EECC55",rock = "#CCBD72",bug ="#C2D21E",
    ghost ="#7975D7",steel ="#C4C2DB",fire ="#FA5643",water ="#56ADFF",grass ="#8CD750",
    electric ="#FDE139",psychic ="#FA65B4",ice ="#96F1FF",dragon ="#8673FF",
    dark ="#8D6855",fairy ="#F9AEFF")



make_card_type = function(title = 'Type',type = 'Grass',id='type1card'){
    

        div(class="typecard", id=id,
            div(class="desctype", type)
        )

}

poke_data <- read.csv('data/all_stats.csv')

poke_names <- poke_data%>%
    select(name)%>%
    pull()%>%
    unique()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    rv <- reactiveValues() ### Auxiliar for all plots of a given pokemon
    rv2 <- reactiveValues() ### Auxiliar for all plots of a given pokemon
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
        
        rv$stats_table <-DT::datatable(df,rownames = FALSE,options = list(searching = FALSE,
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
        
        
        #### Chain plot
        
        
        all_chain_data <- get_chain_info(poke_id)
        
        
        rv$all_chain_data <- all_chain_data    
        
        maxy= all_chain_data$base_stat%>%max()+30
        
        p <- all_chain_data%>%
            arrange(pos)%>%
            mutate(name=forcats::as_factor(name))%>%
            ggplot(aes(name,base_stat))+
            geom_col(position = position_dodge(),alpha=0.6,fill=color)+
            geom_image(aes(image=im1),size=0.35)+
            facet_wrap(~stat,ncol = 3,scales = 'free_x')+
            expand_limits(y = c(0, maxy))+
            dark_mode(ggthemes::theme_hc())+
            theme(strip.background =element_rect(fill="black"))+
            theme(strip.text = element_text(colour = color2,size = 20))+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        
        rv$chain_plot <- p
        
        ### Compare plot 
        

        
        p <- poke_data%>%
            ggplot(aes(stat,base_stat))+
            geom_boxplot(fill=color,alpha=0.6)+
            geom_image(data=poke_stats,aes(image=im1),size=0.2)+
            facet_wrap(~stat,ncol = 3,scales = 'free_x')+
            dark_mode(ggthemes::theme_hc())+
            theme(strip.background =element_rect(fill="black"))+
            theme(strip.text = element_text(colour = color2,size = 20))+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        
        rv$compare_plot <- p
        
        ### update css
        
        session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
        if (color2==color){
            session$sendCustomMessage("hide-type2",color) ## CALL JS FUNCTION    
        }
        session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
        
        
    })
    

    #### Rival pokemon
    observe({
        

        ####
        if (is.null(input$sel2)){
            name = 'bulbasaur'
        }else{
            name= input$sel2
        }
        
        rv2$name=name
        poke_id = poke_data%>%
            filter(name==rv2$name)%>%
            select(pos)%>%
            pull()%>%
            first()
        
        rv2$poke_id <-poke_id
        basic_info = get_usual_info(poke_id)
        rv2$basic_info <- basic_info
        
        color = unname(colors[basic_info$type[1]])
        rv2$color = color
        
        color2 = unname(colors[basic_info$type[2]])
        rv2$color2 = color2
        
        
        type = basic_info$type[1]
        rv2$type = type
        
        type2 = basic_info$type[2]
        rv2$type2 = type2
        
        poke_img_str = poke_data%>%
            filter(name==rv2$name)%>%
            select(im1)%>%
            pull()%>%
            first()
        
        rv2$poke_img_str = poke_img_str
        
        
        poke_stats <- poke_data%>%
            filter(name==rv2$name)
        
        rv2$poke_stats <- poke_stats
        
        ### rival stats table
        
        df <- poke_stats%>%
            select(stat,base_stat)%>%
            rename(name=stat,base=base_stat)
        
        rv2$stats_table <-DT::datatable(df,rownames = FALSE,options = list(searching = FALSE,
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
    })
    
    
    
    output$poke_info_1 <- renderUI({
        

        
        poke_img <- img(src=rv$poke_img_str,width="100%")
        

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
            
            
            
            
            aux <- fluidPage(
                plotOutput('poke_compare')
            )

        } else if (page==2) {
            
            aux <- fluidPage(
                plotOutput('poke_chain')
            )
            
        }else if (page==3) {
            opts = unique(poke_data$stat)
            
            features <- selectInput('sel_feats','Select stats for clustering',
                                    choices = opts  ,multiple=TRUE,selected = opts[0:3] )
            
            aux <- fluidPage(
                fluidRow(column(2,features)),
                fluidRow(column(2,actionButton('runk','Make clusters'))),
                br(),
                plotOutput('poke_clusters')
            )
            
        } else if (page==4) {
            
            aux <- fluidPage(actionButton('getloc','Get location'),
                             h6('This is simulated data, i hope i can get real pokemon data in the future.',
                                style = "color: white;"),
                             leafletOutput('sim_map',height = 900,width = 900))
            
        } else if (page==5) {
            
            aux <- fillPage(
                selectInput('sel2','Select rival your pokemon',choices = poke_names),
                uiOutput('poke_head2')
            )
        }
        
        fillPage(poke_head,aux)
        
    })
    

    output$poke_stats <-DT::renderDataTable({
        
        rv$stats_table
        
    })
    
    output$poke_compare <-renderPlot({
        
        rv$compare_plot
        
    },height = 800,width = 900)
    
    
    output$poke_chain <-renderPlot({
        
        rv$chain_plot  
      
    },height = 800,width = 900)
    
    
    
    ### Update kmeans    
    observeEvent(input$runk,{

        
        
        
        all_chain_names <- rv$all_chain_data%>%
            select(name)%>%
            pull()%>%
            unique()
        
        
        stats <- poke_data%>%
            select(stat,base_stat,name,im1)%>%
            filter(stat%in%input$sel_feats)%>%
            pivot_wider(names_from = stat,values_from=base_stat)%>%
            clean_names()
        
        
        pure_stats <- stats%>%
            select(-name,-im1)
        
        
        ko <- kmeans(pure_stats,5)
        
        

        
        ggp=fviz_cluster(ko, data = pure_stats,
                         geom = "point",
                         ellipse.type = "convex", 
                         ggtheme = theme_bw()
        )
        
        extra_data = ggp$data%>%
            mutate(im1 = stats$im1,r=coord)%>%
            group_by(cluster)%>%
            mutate(r=rank(-r))%>%
            ungroup()%>%
            filter(r<=3)
        
        extra_data2= ggp$data%>%
            mutate(name = stats$name,im1 = stats$im1)%>%
            filter(name %in%all_chain_names)
        
        
        rv$pcluster <- ggp+
            geom_image(data=extra_data,aes(image=im1),size=0.1)+
            geom_image(data=extra_data2,aes(image=im1),size=0.15)

        
    })
    
    output$poke_clusters <- renderPlot({
        
        rv$pcluster
    },height = 1000,width = 1500)
    
    
    ### Location/pokemon go
    observeEvent(input$getloc,{
        session$sendCustomMessage("update_location", 'x')
        
    })
    
    
    observe({

        if(is.null(input$user_lat)){

        }else{
            user_lat=input$user_lat
            user_lon=input$user_lon
            
            set.seed(rv$poke_id)
            
            #lats = runif(100)*0.01 + user_lat -0.005 
            #longs = runif(100)*0.01 + user_lon -0.005 
            
            lats = rnorm(100, mean =user_lat, sd = 0.05) 
            longs = rnorm(100, mean =user_lon, sd = 0.05) 
            
            lats = c(lats, rnorm(1000, mean =user_lat + runif(1)*0.1-0.05, sd = 0.001)  )
            
            longs = c(longs, rnorm(1000, mean =user_lon + runif(1)*0.1-0.05, sd = 0.001)  )
            
            
            data = data.frame(lats,longs)
            
            rv$poke_map <-leaflet(data) %>% 
                addProviderTiles(providers$OpenStreetMap) %>%
                setView(lng = user_lon, lat = user_lat, zoom = 14)%>% addTiles() %>%
                addHeatmap(
                    lng = ~longs, lat = ~lats, intensity = 1,
                    blur = 20, max = 0.05, radius = 15
                )
        }
        
        
        


            
        
    })
    
    output$sim_map <- renderLeaflet({
        rv$poke_map
    })
    
    output$poke_head2 <- renderUI({
        
            
            
            
            poke_img <- img(src=rv2$poke_img_str,width="100%")
            
            
            color = rv2$color
            color2 =rv2$color2
            
            
            type_cards <- fillPage(make_card_type(type = rv2$type,id='typecard2'),
                                   make_card_type(title = 'Sec Type',type = rv2$type2,id='type2card2')
            )
            
             fluidPage(
                fluidRow(column(4,poke_img),
                         column(2,type_cards),
                         column(4,DT::dataTableOutput('poke_stats_2'))),
                plotOutput('onevsone')
            )
    })
    
    output$poke_stats_2 <-DT::renderDataTable({
        
        rv2$stats_table
        
    })
    
    output$onevsone <- renderPlot({
        
        fpoke = rv$poke_stats
        fcolor = rv$color
        spoke = rv2$poke_stats
        scolor = rv2$color
        
        
        both <- rbind(fpoke,spoke)
        
        maxy= both$base_stat%>%max()+30
        
        both%>%
            #arrange(pos)%>%
            mutate(name=forcats::as_factor(name))%>%
            ggplot(aes(name,base_stat,fill=name))+
            geom_col(position = position_dodge(),alpha=0.6)+ 
             scale_colour_manual(values = c(fcolor,scolor),aesthetics = c("colour", "fill"))+
            geom_image(aes(image=im1),size=0.35)+
            facet_wrap(~stat,ncol = 3,scales = 'free_x')+
            expand_limits(y = c(0, maxy))+
            dark_mode(ggthemes::theme_hc())+
            theme(strip.background =element_rect(fill="black"))+
            theme(strip.text = element_text(colour = 'white',size = 20))+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())+ 
            theme(legend.position = "none")
        
        
    },height = 800,width = 900)
    
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
      print(input$crycount)
      session$sendCustomMessage("playcrie", paste0('cries/',rv$poke_id,'.ogg')) ## CALL JS FUNCTION
    })



})

