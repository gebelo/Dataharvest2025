needs(tidyverse,sf,leaflet,mapview)
options(scipen=999)
library(sf)
library(mapview)



uscounties<-read_sf("https://raw.githubusercontent.com/loganpowell/census-geojson/refs/heads/master/GeoJSON/500k/2022/county.json")

mapview(uscounties,
        label = "NAME",
        zcol = "STATEFP",
        legend=FALSE)

election<-read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/refs/heads/master/2024_US_County_Level_Presidential_Results.csv")%>%
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


library(tigris)
counties_joined <- geo_join(uscounties, dhdemos, "GEOID","GEOID")

glimpse(counties_joined)

my_pal <- colorNumeric("Blues", domain=counties_joined$college)


leaflet()%>%
  addTiles()%>%
  addPolygons(data = counties_joined , 
              fillColor =~my_pal(counties_joined$college), 
              fillOpacity = 0.75, 
              weight = 0.1, 
              smoothFactor = 0.1, 
              popup = ~NAME) %>%
  setView(-98.483330, 38.712046, zoom = 4) 


summer21 <- read_csv("~/Library/CloudStorage/GoogleDrive-rgebeloff@nytimes.com/My Drive/gebeloff/Public/census2023/guns/incidents_0612updateFIXED.csv")%>%
 filter(year==2021 & month %in% c(6,7,8))%>%
  select(incident_id,date=Date,address,lat=newlat,lon=newlon,killed,incident_time)

write_csv(summer21, "summer21.csv")

leaflet(summer21) %>%
  addTiles() %>% 
  setView(-98.483330, 38.712046, zoom = 4)%>%
  addCircles(~summer21$lon, ~summer21$lat, 
             popup=summer21$killed, weight = 3, radius=40, 
             color="red", stroke = TRUE, fillOpacity = 0.8) 

library(sf)

uscounties<-uscounties%>%
  st_transform(crs = 4326)

summer21<-summer21%>%
  st_as_sf(
    coords = c("lon",
               "lat"),
    crs = 4326)


shootings_joined <-shootings%>%
  #set the coordinates for the shootings
  st_as_sf(
    coords = c("lon",
               "lat"),
    crs = 4326) %>%
  #join the shootings to the tracts file
  st_join(cooktracts,
          join = st_intersects, 
          left = TRUE)

killing_summary<-shootings_joined%>%
  as_tibble()%>%
  group_by(GEOID)%>%
  summarize(totkilled=sum(killed))

demographic_summary<-chicagopop%>%
  mutate(GEOID=as.character(GEOID))%>%
  left_join(killing_summary)%>%
  mutate(totkilled=ifelse(is.na(totkilled),0,totkilled))%>%
  mutate(percap=(totkilled/pop)*1000)

cooktracts_joined<-cooktracts%>%
  inner_join(demographic_summary)


my_pal <- colorBin("Greens", domain=cooktracts_joined$percap, bins=7)

leaflet(shootings) %>%
  addTiles() %>% 
  setView(-87.6298, 41.8781, zoom = 7) %>% 
  addPolygons(data = cooktracts_joined , 
              fillColor =~my_pal(cooktracts_joined$percap), 
              fillOpacity = 0.9, 
              weight = 0.1, 
              smoothFactor = 0.1, 
              popup = ~cooktracts_joined$percap)%>%
  addCircles(~lon, ~lat, 
             popup=shootings$killed, weight = 3, radius=40, 
             color="red", stroke = TRUE, fillOpacity = 0.8) 

uscountiespop<-counties_joined%>%
  select(GEOID,NAME,STATE_NAME, pop,geometry)
  

