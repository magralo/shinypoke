
library(httr)
library(jsonlite)
library(tidyverse)
library(ggimage)


get_basic_info <- function (id){
  
  url <- paste0("https://pokeapi.co/api/v2/pokemon/",id)
  
  res = GET(url)
  
  data <- fromJSON(rawToChar(res$content))
  
  image1 <- data$sprites$other$`official-artwork`$front_default
  
  image2 <- data$sprites$other$dream_world$front_default
  
  name <- data$name
  
  stats <- data.frame(data$stats$stat)%>%
    rename(stat=name)
  
  stats$base_stat <- data$stats$base_stat
  
  all <- stats
  
  all$name <- name
  
  all$im1 <- image1
  
  all$im2 <- image2
  
  
  return(all)
  
}



get_chain_info <- function (id){
  
  
  url <- paste0("https://pokeapi.co/api/v2/pokemon-species/",id)
  
  res <- GET(url) 
  
  data <- fromJSON(rawToChar(res$content))
  
  url_evol <- data$evolution_chain$url
  
  res <- GET(url_evol) 
  
  data <- fromJSON(rawToChar(res$content))
  
  chain <- data$chain
  links <- chain$species$url
  
  
  while (length((chain$evolves_to))>0){
    aux <- chain$evolves_to
    if(length(aux) >1 ){
      links <- c(links,aux$species$url)
    }else{
      links <- c(links,aux[[1]]$species$url)
    }
    chain <- chain$evolves_to
  }
  
  data <- data.frame()
  i=1
  for (s in links){
    number <-  strsplit(s,'pokemon-species')[[1]][2]
    number <-  strsplit(number,'/')[[1]][2]
    aux <- get_basic_info(number)
    print(s)
    data <- data%>%
      bind_rows(mutate(aux,pos=i))
    i=i+1
  }
  
  
  return(data)
  
}



get_usual_info<- function (id){
  
  
  url <- paste0("https://pokeapi.co/api/v2/pokemon/",id)
  
  res <- GET(url) 
  
  data <- fromJSON(rawToChar(res$content))
  
  type <- c(data$types$type$name,data$types$type$name)[1:2]
  
  height <- data$height
  weight <- data$weight
    
  return(list(type=type,height=height,weight=weight))
  
}
