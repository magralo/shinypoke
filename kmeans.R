library(tidyverse)
library(janitor)
library("ggimage")

data <- read.csv('data/all_stats.csv')
head(data)


stats <- data%>%
  select(stat,base_stat,name,im1)%>%
  pivot_wider(names_from = stat,values_from=base_stat)%>%
  clean_names()


pure_stats <- stats%>%
  select(-name,-im1)


ko <- kmeans(pure_stats,5)


#install.packages('factoextra')

library(factoextra)

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
  mutate(name = stats$name)%>%
  filter(name %in%llll)
  

ggp+
  geom_image(data=extra_data,aes(image=im1),size=0.1)+
  geom_image(data=extra_data2,aes(image=im1),size=0.1)




info_chain=get_chain_info(148)




maxy= info_chain$base_stat%>%max()+30

info_chain%>%
  arrange(pos)%>%
  mutate(name=forcats::as_factor(name))%>%
  ggplot(aes(name,base_stat))+
  geom_col(alpha=0.4)+
  geom_image(aes(image=im1),size=0.4)+
  facet_wrap(~stat)+
  expand_limits(y = c(0, maxy))+
  ggthemes::theme_hc()


###




data%>%
  ggplot(aes(stat,base_stat))+
  geom_boxplot(fill='lightblue',alpha=0.5)+
  geom_image(data=info_chain,aes(image=im1),size=0.1)+
  ggthemes::theme_hc()
  
  


