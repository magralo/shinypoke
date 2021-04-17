



fightUI <- function(id) {
  
  fillPage(
    selectInput(NS(id,'sel2'),'Select rival your pokemon',choices = poke_names),
    uiOutput(NS(id,'poke_head2'))
  )
  
}


fightServer <- function(id,poke_data,color_poke1,stats_poke1) {
  
  
  moduleServer(id, function(input, output, session) {
    

    rv2 <- reactiveValues() ### Auxiliar for all plots of a given pokemon
    
    observe({
      
      poke_data<-poke_data()
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
    
    
    
    
    
    output$poke_head2 <- renderUI({
      

      poke_img <- img(src=rv2$poke_img_str,width="50%")
      color = rv2$color
      color2 =rv2$color2
      
      
      type_cards <- fillPage(make_card_type(type = rv2$type,id='typecard2'),
                             make_card_type(title = 'Sec Type',type = rv2$type2,id='type2card2')
      )
      
      fluidPage(
        fluidRow(column(4,poke_img),
                 column(2,type_cards),
                 column(4,DT::dataTableOutput(NS(id,'poke_stats_2')))),
        actionButton(NS(id,'compare'),'Make bars'),
        plotOutput(NS(id,'onevsone'))
      )
    })
    
    
    output$poke_stats_2 <-DT::renderDataTable({
      
      rv2$stats_table
      
    })
    
    observeEvent(input$compare,{
      
      fpoke = stats_poke1()
      fcolor = color_poke1()
      spoke = rv2$poke_stats
      scolor = rv2$color
      
      
      both <- rbind(fpoke,spoke)
      
      maxy= both$base_stat%>%max()+30
      
      p <- both%>%
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
      
      rv2$plot <- p
      
    })
    
    output$onevsone <- renderPlot({
      
      rv2$plot
      
      
    })
    
    
    ### Solve reactivity issues for the background
    
    timer <- reactiveTimer(500)
    
    
    observe({
      
      timer()

      color = rv2$color
      color2 = rv2$color2
      
      session$sendCustomMessage("background-type-rival", color) ## CALL JS FUNCTION
      if (color2==color){
        session$sendCustomMessage("hide-type2-rival",color) ## CALL JS FUNCTION    
      }
      session$sendCustomMessage("background-type2-rival", color2) ## CALL JS FUNCTION
        

      
    })
    
    
    
    reactive({
      
      input$sel2
      
    })
    
    
    
  })
}
