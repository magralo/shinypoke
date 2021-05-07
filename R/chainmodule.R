

chainUI <- function(id) {
  fluidPage(
    actionButton(NS(id,'runmodule'),label = "Make chain plots"),
    plotOutput(NS(id,'poke_chain'),width = "40%")
  )
}


chainServer <- function(id,poke_id,color,color2,getancho) {
  stopifnot(is.reactive(poke_id))
  stopifnot(is.reactive(color))
  stopifnot(is.reactive(color2))

  
  moduleServer(id, function(input, output, session) {
    

    
    chaindata <- eventReactive(input$runmodule,{
      
      get_chain_info(poke_id())
      
      
    })
    
    output$poke_chain <-renderPlot({
      print(getancho())
      if (!is.null(chaindata())){
        all_chain_data  <- chaindata()
        
        maxy= all_chain_data$base_stat%>%max()+30
        
        all_chain_data%>%
          arrange(pos)%>%
          mutate(name=forcats::as_factor(name))%>%
          ggplot(aes(name,base_stat))+
          geom_col(position = position_dodge(),alpha=0.6,fill=color())+
          geom_image(aes(image=im1),size=0.35)+
          facet_wrap(~stat,ncol = 3,scales = 'free_x')+
          expand_limits(y = c(0, maxy))+
          dark_mode(ggthemes::theme_hc())+
          theme(strip.background =element_rect(fill="black"))+
          theme(strip.text = element_text(colour = color2(),size = 20))+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      }else{
        
      }
      
      
      
    },width =  function() getancho() * 0.6, height = function() getancho() * 0.3)
    
    
    reactive({

      chaindata()
      
    })

    
  })
}