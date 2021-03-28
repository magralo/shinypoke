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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(page='red')
    observe({
        rv$page=input$prueba
    })
    
    output$poke_info_1 <- renderUI({

        poke_img_str = poke_data%>%
            filter(name==input$sel1)%>%
            select(im1)%>%
            pull()
        
        poke_img <- img(src=poke_img_str[1],width="100%")
        
        
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()
        
        basic_info <- get_usual_info(poke_id[1])
        color = unname(colors[basic_info$type[1]])
        color2 = unname(colors[basic_info$type[2]])
        
        
        type_cards <- fillPage(make_card_type(type = basic_info$type[1]),
                                make_card_type(title = 'Sec Type',type = basic_info$type[2],id='type2card')
                                )
        
        poke_head <- fillPage(
            fluidRow(column(4,poke_img),
                     column(2,type_cards),
                     column(4,DT::dataTableOutput('poke_stats')))
        )
        
        session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
        session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
        
        
         
        
        if (rv$page=='green'){
            
            
            
            
            aux <- fluidPage()

        } else if (rv$page=='yellow') {
            
            aux <- fluidPage(
                plotOutput('poke_chain')
            )
            
        }else if (rv$page=='black') {
            opts = unique(poke_data$stat)
            
            features <- selectInput('sel_feats','Select stats for clustering',
                                    choices = opts  ,multiple=TRUE,selected = opts[0:3] )
            
            aux <- fluidPage(
                fluidRow(column(2,features)),
                fluidRow(column(2,actionButton('runk','Make clusters'))),
                br(),
                plotOutput('poke_clusters')
            )
            
        } else {
            aux <- fillPage(
                plotOutput('poke_compare')
            )
            
        }
        
        fillPage(poke_head,aux)
        
    })
    
    
    
    
    #output$poke_stats <-renderPlotly({
    output$poke_stats <-DT::renderDataTable({
        
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()
        
        basic_info <- get_usual_info(poke_id[1])
        color = unname(colors[basic_info$type[1]])
        
        
        p <- poke_data%>%
                filter(name==input$sel1)%>%
                ggplot(aes(stat,base_stat,fill=stat))+
                geom_col()+
                dark_mode(ggthemes::theme_hc())
        

        df <- poke_data%>%
            filter(name==input$sel1)%>%
            select(stat,base_stat)%>%
            rename(name=stat,base=base_stat)
        
        DT::datatable(df,rownames = FALSE,options = list(searching = FALSE,
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
    
    output$poke_compare <-renderPlot({
        
        
        
        chain_data <- poke_data%>%
            filter(name==input$sel1)
        
        
        p <- poke_data%>%
            ggplot(aes(stat,base_stat,fill=stat))+
            geom_boxplot()+
            geom_image(data=chain_data,aes(image=im1),size=0.2)+
            facet_wrap(~stat,ncol = 3,scales = 'free_x')+
            dark_mode(ggthemes::theme_hc())
        
        p
    },height = 800,width = 900)
    
    
    output$poke_chain <-renderPlot({
        
        
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()%>%
            first()%>%
            as.numeric()
        
        

        basic_info <- get_usual_info(poke_id)
        color = unname(colors[basic_info$type[1]])
        

        all_chain_data <- get_chain_info(poke_id)

        


        p <- all_chain_data%>%
            ggplot(aes(name,base_stat))+
            geom_col(position = position_dodge(),alpha=0.6,fill=color)+
            geom_image(aes(image=im1),size=0.35)+
            facet_wrap(~stat,ncol = 3,scales = 'free_x')+
            dark_mode(ggthemes::theme_hc())
        
        p
    },height = 800,width = 900)
    
    

    
    observeEvent(input$runk,{
        print('hello')
        print(input$sel_feats)
        
        
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()%>%
            first()%>%
            as.numeric()
        
        
        all_chain_names <- get_chain_info(poke_id)%>%
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
    
    
    ### Solve reactivity issues for the background
    
    timer <- reactiveTimer(500)
    
    
    observe({
        
        timer()
        poke_id = poke_data%>%
            filter(name==input$sel1)%>%
            select(pos)%>%
            pull()
        
        basic_info <- get_usual_info(poke_id[1])
        color = unname(colors[basic_info$type[1]])
        color2 = unname(colors[basic_info$type[2]])
        
        session$sendCustomMessage("background-type", color) ## CALL JS FUNCTION
        if (color2==color){
            print(1782)
            session$sendCustomMessage("hide-type2",color) ## CALL JS FUNCTION    
        }
        session$sendCustomMessage("background-type2", color2) ## CALL JS FUNCTION
        
        
    })



})

