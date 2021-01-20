library(sf)          
library(raster)
library(tidyverse)

#Load the spatial polygon data for BC Parks and Protected Areas
park_boundaries<-st_read("Data/TA_PEP_SVW_polygon.shp")

park_boundaries %>%
  as_tibble() %>%
  arrange(desc(AREA_SQM)) %>%
  group_by(PROT_DESG, PARK_CLASS) %>%
  summarize(n=n(),
            AREA=sum(AREA_SQM)/10000) %>%
  arrange(desc(n)) %>%
  ungroup()%>%
  summarize(total=sum(n),
            area=sum(AREA))

park_boundaries %>%
  as_tibble() %>%
  filter(PROT_DESG=="PROVINCIAL PARK" & PARK_CLASS=="Class A") %>%
  arrange(PROT_NAME) %>%
  dplyr::select(PROT_NAME,ORC_PRIMRY,ORC_SCNDRY,F_CODE,AREA_SQM) %>%
  group_by(ORC_PRIMRY) %>%
  summarise(n=n(),
            area=sum(AREA_SQM)/10000,
            name=first(PROT_NAME)) %>%
  arrange(name) %>%
  top_n(10,wt=area) %>%
  mutate(name=reorder(name,area)) %>%
  ggplot(aes(name,area))+
  geom_col(fill="forestgreen")+
  coord_flip() +
  theme_minimal()+
  ylab("area (km^2)")+
  xlab(NULL)+
  ggtitle("10 Largest Provincial Parks in British Columbia")
  



  group_by(PROT_NAME,PROT_DESG,PARK_CLASS) %>%
  summarize(n=n(),
            AREA=sum(AREA_SQM)/10000)
  
  
  
