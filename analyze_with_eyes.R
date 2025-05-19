
options(scipen=999)

setwd("/workspaces/Dataharvest2025")

#install.packages("tidyverse")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("sf")
#install.packages("leaflet")
#install.packages("tigris")
#install.packages("ggridges")
#install.packages("formattable")

library(tidyverse)
library(corrplot)
library(GGally)
library(sf)
library(leaflet)
library(tigris)
library(ggridges)
library(formattable)

election<-read_csv("voting24.csv")%>%
  select(state_name,county_fips,trump=per_gop)

dhdemos<-read_csv("dhdemos.csv")%>%
  mutate(GEOID=Geo_FIPS,
         state=Geo_STUSAB,
         county=Geo_NAME,
         pop=SE_A00002_001,
         density=SE_A00002_002,
         med_age=SE_A01004_001,
         white=SE_A04001_003/SE_A04001_001,
         black=SE_A04001_004/SE_A04001_001,
         nativeam=SE_A04001_005/SE_A04001_001,
         asian=(SE_A04001_006+SE_A04001_007)/SE_A04001_001,
         hispanic=SE_A04001_010/SE_A04001_001,
         married_hh=SE_A10008_003/SE_A10008_001,
         hhsize=SE_A10003_001,
         college=SE_A12001_005/SE_A12001_001,
         nilf=SE_A17002_007/SE_A17002_001,
         unemp=SE_A17002_006/SE_A17002_004,
         income=SE_A14006_001,
         retirement=SE_A10017_002/SE_A10017_001,
         owned=SE_A10060_002/SE_A10060_001,
         yearbuilt=SE_A10057_001,
         rent=SE_A18009_001,
         commute=SE_A09003_001,
         noncit=SE_A06001_004/SE_A06001_001,
         uninsured=SE_A20001_002/SE_A20001_001,
         singleparent=SE_A10065_002/SE_A10065_001)%>%
  select(GEOID:singleparent)%>%
  filter(state!="pr")%>%
  left_join(election, by=c("GEOID"="county_fips"))%>%
  relocate(state_name, .after="GEOID")

glimpse(dhdemos)

ggplot(data = dhdemos) + 
  geom_point(mapping = aes(x = pop, y = trump))

dhdemos%>%filter(is.na(trump))%>%
  group_by(state)%>%
  summarize(count=n())

dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_point(mapping = aes(x = pop, y = trump))+
  geom_smooth(mapping = aes(x = pop, y = trump))

dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_point(mapping = aes(x = pop, y = trump))+
  geom_smooth(mapping = aes(x = pop, y = trump))+
  xlim(0,750000)

dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_point(mapping = aes(x = pop, y = trump))+
  geom_smooth(mapping = aes(x = pop, y = trump))+
  xlim(750000,max(dhdemos$pop))

dhnumbers<-dhdemos%>%
  select(pop:trump)

M = cor(dhnumbers,use="complete.obs")

corrplot(M, method = 'color')


corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')

corrplot(M, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')


pro_trump<-dhdemos%>%
  select(GEOID:county, trump,married_hh,owned,white,nilf,med_age,retirement)

smoothing <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

ggpairs(pro_trump, columns=5:11,lower = list(continuous = smoothing)) 


dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_point(mapping = aes(x = white, y = trump))+
  geom_smooth(mapping = aes(x = white, y = trump))


dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_smooth(mapping = aes(x = white, y = trump))+
  labs(title = "Trump Voting by %White Pop",
       y = "Trump Vote%", x = "Pct White") + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  facet_wrap(~ state_name)  +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0))
  )

dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_smooth(mapping = aes(x = college, y = trump))+
  labs(title = "Trump Voting by %College Educated",
       y = "Trump Vote%", x = "Pct College Educated") + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  facet_wrap(~ state_name)  +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0))
  )

countytypes<-read_csv("countytypes.csv")%>%
  inner_join(election, by=c("FIPStxt"="county_fips"))%>%
  filter(Attribute=="Industry_Dependence_2025")%>%
  mutate(econ_type=case_when(
    Value==0 ~ 'Not dependent',
    Value==1 ~ 'Farming dependent',
    Value==2 ~ 'Mining dependent',
    Value==3 ~ 'Manufacturing dependent',
    Value==4 ~ 'Government dependent',
    Value==5 ~ 'Recreation dependent',
    Value==99 ~ "N/A"))

countytypes%>%
  ggplot()+
  aes(x = str_wrap(as.factor(econ_type),width = 10), y = trump) +
  geom_boxplot() +
  labs(title = "Trump Voting by County Economic Type",
       y = "Trump Vote%", x = "County Type")

countypops<-dhdemos%>%
  select(GEOID,pop)

countytypes<-read_csv("countytypes.csv")%>%
  inner_join(election, by=c("FIPStxt"="county_fips"))%>%
  inner_join(countypops, by=c("FIPStxt"="GEOID"))%>%
  filter(Attribute=="Industry_Dependence_2025")%>%
  mutate(econ_type=case_when(
    Value==0 ~ 'Not dependent',
    Value==1 ~ 'Farming dependent',
    Value==2 ~ 'Mining dependent',
    Value==3 ~ 'Manufacturing dependent',
    Value==4 ~ 'Government dependent',
    Value==5 ~ 'Recreation dependent',
    Value==99 ~ "N/A"))

countytypes%>%
  ggplot()+
  aes(x = str_wrap(as.factor(econ_type),width = 10), weight=pop,y = trump) +
  geom_boxplot() +
  labs(title = "Trump Voting by County Economic Type (Weighted by Pop.)",
       y = "Trump Vote%", x = "County Type")

countytypes%>%
  ggplot()+ 
  aes(x=str_wrap(as.factor(econ_type),width = 14), y=trump, fill=econ_type,
      weight=pop) +
  geom_violin()

ggplot(countytypes, aes(trump, fct_rev(econ_type), color = econ_type, fill = econ_type)) + 
  coord_cartesian(clip = "off")+
  geom_density_ridges(
    alpha = .7)

ggplot(countytypes, 
       aes(x = trump, y = fct_rev(econ_type))) +
  geom_density_ridges(aes(height=..density..,  
                          weight=pop, color = econ_type, fill = econ_type), 
                      scale= 0.7,
                      stat="density")+ 
  coord_cartesian(clip = "off") 

uscounties<-read_sf("https://raw.githubusercontent.com/loganpowell/census-geojson/refs/heads/master/GeoJSON/500k/2022/county.json")

themap<-leaflet()%>%
  addTiles()%>%
  addPolygons(data = uscounties , 
              fillColor = "yellow", 
              fillOpacity = 0.5, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~NAME) %>%
  setView(-98.483330, 38.712046, zoom = 3) 

counties_joined<-uscounties%>%
  inner_join(dhdemos, by=c("GEOID"))

mybins <- c(0, 0.3, 0.4, 0.5, 0.6,0.7,Inf)

mypalette <- colorBin(
  palette = "YlOrRd", domain =counties_joined$trump,
  na.color = "transparent", bins = mybins
)

popup<- paste0("Trump Pct: ", percent(counties_joined$trump,0))

themap4<-leaflet() %>%
  addTiles() %>%
  setView(-98.483330, 38.712046, zoom = 3) %>%
  addPolygons(
    data=counties_joined,
    fillColor = ~ mypalette(counties_joined$trump),
    stroke = F,
    fillOpacity = 0.50,
    label = popup,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "11px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = mypalette, values = counties_joined$trump, opacity = 0.9,
    title = "Trump Voting", position = "bottomleft"
  )
