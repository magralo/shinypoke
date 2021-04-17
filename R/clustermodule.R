



clusterUI <- function(id) {
  
  opts = unique(poke_data$stat)
  
  features <- selectInput(NS(id,'sel_feats'),'Select stats for clustering',
                          choices = opts  ,multiple=TRUE,selected = opts[0:3] )
  
  fluidPage(
    fluidRow(column(2,features)),
    fluidRow(column(2,actionButton(NS(id,'runk'),'Make clusters'))),
    br(),
    plotOutput(NS(id,'poke_clusters'))
  )
  
}


clusterServer <- function(id,pokemon) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    rv <- reactiveValues() ### Auxiliar for this module
 
    observeEvent(input$runk,{
      
      
      #all_chain_data <- get_chain_info(poke_id)
      print('cluster')
      all_chain_names <- get_chain_info(pokemon())%>%
        select(name)%>%
        pull()%>%
        unique()
      
      print('ok chain')
      
      
      stats <- poke_data%>%
        select(stat,base_stat,name,im1)%>%
        filter(stat%in%input$sel_feats)%>%
        pivot_wider(names_from = stat,values_from=base_stat)%>%
        clean_names()
      
      print('ok stats')
      
      pure_stats <- stats%>%
        select(-name,-im1)
      
      print('ok pure stats')
      
      print(head(pure_stats))
      
      print(input$sel_feats)
      
      ko <- kmeans(pure_stats,5)
      
      print('ok ko')
      
      ggp=fviz_cluster(ko, data = pure_stats,
                       geom = "point",
                       ellipse.type = "convex", 
                       ggtheme = theme_bw()
      )
      
      print('ggp')
      
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
    })
    
    
    
  })
}
