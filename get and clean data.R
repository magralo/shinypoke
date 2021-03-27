library(httr)
library(jsonlite)
library(tidyverse)
library("ggimage")


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





data <- data.frame()
for (i in 1:386){

  aux <- get_basic_info(i)
  data <- data%>%
    bind_rows(mutate(aux,pos=i))
  i=i+1
}

write.csv(data,'data/all_stats.csv',row.names = FALSE)

