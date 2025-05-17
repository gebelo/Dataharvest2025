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
  

dhdemos%>%filter(is.na(trump))%>%
  group_by(state)%>%
  summarize(count=n())

dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  geom_point(mapping = aes(x = pop, y = trump))+
  geom_smooth(mapping = aes(x = pop, y = trump))



library(corrplot)

dhnumbers<-dhdemos%>%
  select(pop:trump)

M = cor(dhnumbers,use="complete.obs")

corrplot(M, method = 'color')


library(GGally)

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

ggpairs(swiss,columns = 1:4, lower = list(continuous = my_fn))


dhdemos%>%
  filter(!is.na(trump))%>%
  ggplot + 
  aes(white, trump) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Trump Voting by %White Pop",
       y = "Trump Vote%", x = "Pct White") + 
  facet_wrap(~ state_name)  +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0))
  )

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

election<-read_csv("voting24.csv")%>%
  select(state_name,county_fips,trump=per_gop)

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
  aes(x = str_wrap(as.factor(econ_type),width = 10), y = trump) +
  geom_boxplot() +
  labs(title = "Trump Voting by County Economic Type",
       y = "Trump Vote%", x = "County Type")

countytypes%>%
  ggplot()+ 
  aes(x=str_wrap(as.factor(econ_type),width = 8), y=trump, fill=econ_type,
                weight=pop) +
  geom_violin()

library(ggridges)


g_ridges <- 
  ggplot(countytypes, aes(trump, fct_rev(econ_type), color = econ_type, fill = econ_type)) + 
  coord_cartesian(clip = "off")+
   geom_density_ridges(
    alpha = .7, size = 1.5
  )

Economic Typology Codes	
High_Farming_2025	High farming-concentration counties: Farming accounted for at least 20 percent of the county's earnings or 17 percent of the county's jobs averaged over 2019, 2021, and 2022. 0 = no; 1 = yes; 99 = not available
High_Mining_2025	High mining-concentration counties: Mining accounted for at least 11 percent of the county's earnings or 7 percent of the county's jobs averaged over 2019, 2021, and 2022.  0 = no; 1 = yes; 99 = not available
High_Manufacturing_2025	High manufacturing-concentration counties: Manufacturing accounted for at least 25 percent of the county's earnings or 17 percent of the county's jobs averaged over 2019, 2021, and 2022.  0 = no; 1 = yes; 99 = not available
High_Government_2025	High government-concentration counties: Federal and State Government accounted for at least 13 percent of the county's earnings or 8 percent of the county's jobs averaged over 2019, 2021, and 2022.  0 = no; 1 = yes; 99 = not available
High_Recreation_2025	High recreation-concentration counties: Weighted z-score of recreation earnings, jobs, and housing greater than or equal to 0.67. See the documentation on the ERS website for more information (Data Products > County Typology Codes > Documentation).  0 = no; 1 = yes; 99 = not available
Nonspecialized_2025	Nonspecialized counties: Counties were not classified as having a 'high concentration' in farming, mining, manufacturing, government, or recreation.  0 = no; 1 = yes; 99 = not available
Industry_Dependence_2025	Mutually exclusive economic dependence county indicator: Indicates the industry with a weighted z-score greater than or equal to '1' and that is higher than any other industry's weighted z-score. See the Industry Dependence Code Thresholds section of the documentation on the ERS website for more information (Data Products > County Typology Codes > Documentation). 0 = Not dependent; 1 = Farming dependent; 2 = Mining dependent; 3 = Manufacturing dependent; 4 = Government dependent; 5 = Recreation dependent; 99 = not available
	
Demographic Typology Codes	
Low_PostSecondary_Ed_2025	Low post-secondary education counties: At least 57 percent of residents ages 25 to 64 did not have any post-secondary education in 2018–22 5-year American Community Survey (ACS). 0 = no; 1 = yes; 99 = not available
Low_Employment_2025	Low employment counties: Less than 63 percent of residents ages 25 to 54 were employed in 2018–22 5-year ACS. 0 = no; 1 = yes; 99 = not available
Population_Loss_2025	Population loss counties: Number of residents declined from 2000 to 2010 and from 2010 to 2020, based on Decennial Census data. 0 = no; 1 = yes; 99 = not available
Housing_Stress_2025	Housing stress counties: At least 29 percent of owner- and renter-occupied housing units had at least one of the following conditions: 1) lacking complete plumbing facilities, 2) lacking complete kitchen facilities, 3) with 1.01 or more occupants per room, 4) selected monthly owner costs as a percentage of household income greater than 30 percent, and 5) gross rent as a percentage of household income greater than 30 percent. As defined in table B25123 of the 2018–22 5-year ACS. 0 = no; 1 = yes; 99 = not available
Retirement_Destination_2025	Retirement destination counties: Number of residents ages 55 to 74 increased by at least 15 percent from 2010 to 2020 because of migration, based on data from the University of Wisconsin - Madison's Applied Population Laboratory. 0 = no; 1 = yes; 99 = not available
Persistent_Poverty_1721	Persistent poverty counties: Poverty rate was 20 percent or higher in the 1990 and 2000 Decennial Censuses and in the 2007–11 and 2017–21 5-year ACS. See the Poverty Area Measures data product on the ERS website for more information (Data Products > Poverty Area Measures) for more information. -1 = not determined; 0 = no; 1 = yes; 99 = not available
