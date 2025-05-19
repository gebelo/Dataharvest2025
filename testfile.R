library(tidyverse)
library(corrplot)
library(GGally)
library(sf)
library(leaflet)
library(tigris)
library(ggridges)
library(formattable)
options(scipen=999)

setwd("/workspaces/Dataharvest2025")

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

themap


dhnumbers<-dhdemos%>%
  select(pop:trump)

M = cor(dhnumbers,use="complete.obs")

corrplot(M, method = 'color')

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
