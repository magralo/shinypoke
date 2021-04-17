

predUI <- function(id) {
  
  loadimage <- fileInput(NS(id,"poke_upload"), "Choose pokemon image",
                                 multiple = FALSE,
                                 accept = c("jpg"))
  
  fluidPage(loadimage,
                   actionButton(NS(id,'getpred'),'Whos dat'),
                   uiOutput(NS(id,'poke_predui'))
  )
  
}



predServer <- function(id,poke_data) {


  moduleServer(id, function(input, output, session) {
    
    rvpred <- reactiveValues(file = 'none')
    
    output$poke_predui <- renderUI({
      if (!is.null(input$poke_upload$datapath)){
        fluidRow(column(3,imageOutput(NS(id,"userImage"))),
                 column(4,plotOutput(NS(id,'poke_predictions'),width = "50%"))
        )
      }else {
        fluidPage()
      }
    })
    
    output$userImage <- renderImage({
      list(src = input$poke_upload$datapath,
           alt = "Image failed to render",
           id= "userpoke")
    }, deleteFile = FALSE)
    
    observeEvent(input$getpred,{
    print('QHUBO')
      
      if (!is.null(input$poke_upload$datapath)){
        
        run = FALSE
        
        
        
        if(rvpred$file != input$poke_upload$datapath){
          
          shinyalert(title = "Running! make take a while", type = "warning")
          
          
          
          rvpred$file = input$poke_upload$datapath
          
          proj <- Sys.getenv("GCS_PROJ")
          
          image <- paste0(nrow(googleCloudStorageR::gcs_list_objects()),'-',round(runif(1)*1000),'.jpg')
          
          print('uploading')
          gcs_upload(input$poke_upload$datapath,name=image,predefinedAcl = "bucketLevel")
          print('uploaded')
          #gcs_upload('sample_images/groudon2.jpg',name=image,predefinedAcl = "bucketLevel")
          
          print('requesting')
          print(Sys.time())
          
          
          url <- paste0(Sys.getenv("pokeapi"),image)
          
          res = GET(url)
          
          print('ok')
          print(Sys.time())
          
          poke_prediction <- fromJSON(rawToChar(res$content))
          
          print(poke_prediction)
          #preds = poke_prediction(input$poke_upload$datapath)%>% #using python
          preds = poke_prediction%>% #using python
            inner_join(poke_data(),by=c('label'='name'))%>%
            filter(stat=='hp')%>%
            select(label,pos,prob,im1)%>%
            arrange(-prob)%>%
            rename(name=label)%>%
            mutate(size = ifelse(prob==max(prob),0.3,0.2))
          
          poke_pred<- head(preds$name,1)
          
          maxy= preds$prob%>%max()+0.1
          
          rvpred$plot <- preds%>%
            mutate(name=forcats::as_factor(name))%>%
            ggplot(aes(name,prob))+
            geom_col(alpha=0.6)+
            geom_image(aes(image=im1),size=preds$size)+
            expand_limits(y = c(-0.1, maxy))+
            dark_mode(ggthemes::theme_hc())+
            theme(axis.title.x=element_blank())
          
          rvpred$poke_pred<- poke_pred
          
          updateVarSelectInput(session,"sel1",selected = poke_pred)
          
          #shinyalert(title = "Done",type='success')
          
          shinyalert(title = glue::glue('I think your pokemon is {poke_pred}'),type = 'info')
          
        }
        
        
        
      } 
      
    })
    
    observe({
      
      print(input$poke_upload$datapath)
      
      rvpred$plot = NULL
      
    })
    
    output$poke_predictions <-renderPlot({
      print('plot')
      print(Sys.time())
      rvpred$plot
      
    })
    
    
    
    
    reactive({
      
      rvpred$poke_pred
      
    })
    
    
  })
}