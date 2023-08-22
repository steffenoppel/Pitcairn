################################################################################################################
#############  POPULATION SIZE ASSESSMENT FOR PITCAIRN REED WARBLER   ##########################################
################################################################################################################

### based on Montserrat Oriole approach published in 2014 (Oppel et al. 2014, BCI)
### written by steffen.oppel@rspb.org.uk

### updated on 16 Jan 2023 to extrapolate total abundance across island

### updated 14 Feb after modifications to database


#### REVISION IN AUGUST 2023
## include GOF Test
## subsample transects to remove perceived 'pseudoreplication'

######################################################################################
#############  Load required packages       ##########################################
######################################################################################

library(reshape)
library(tidyverse)
library(lubridate)

library(maptools)
library(sf)
library(ggmap)
library(ggsn)
library(ggspatial)

library(unmarked)
library(geosphere)
library(janitor)
library(AICcmodavg) ### load for goodness of fit test
filter<-dplyr::filter
select<-dplyr::select
rename<-dplyr::rename



######################################################################################
#############  Import data from tables in the Access Database        ############
######################################################################################
# setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
# setwd("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
setwd("C:/Users/sop/Documents/Steffen/RSPB/Pitcairn/Science/DATA")
#setwd("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
# system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
# load("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
# load("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
load("C:/Users/sop/Documents/Steffen/RSPB/Pitcairn/Science/DATA/PIRW_data.RData")
ls()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TRANSCRIBE THE HABITAT INFORMATION FROM ALL TRANSECT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(transects)
points<- transects %>% filter(Type=="Point") %>%
  select(-Type,-Slope,-Length,-Start_description,-End_description,-Biome,-IslandPart,-AreaName,-Lat_end,-Long_end)
head(transects)
transects <- transects %>% filter(Type=="Transect") %>%
  select(-Type,-Slope,-Length,-Start_description,-End_description,-Biome,-IslandPart,-AreaName,-Habitat_description)
head(transects)
#unique(transects$Habitat_description)

## split one column and then process the data
## removed in Feb 2023 when this was included in database
# transects[,7:10]<-data.table::tstrsplit(transects$Habitat_description, split="% ",fill=0)
# 
# habitats<- transects %>% mutate(hab1=str_replace_all(V7,pattern="%",replacement="")) %>%
#   mutate(hab2=str_replace_all(V8,pattern="%",replacement="")) %>% 
#   mutate(hab3=str_replace_all(V9,pattern="%",replacement="")) %>% 
#   mutate(hab4=str_replace_all(V10,pattern="%",replacement="")) %>%
#   select(Transect, hab1,hab2,hab3,hab4) %>%
#   gather(key=Habitat, value=comp,-Transect) %>%
#   filter(comp!="0") %>%
#   select(Transect,comp) %>%
#   separate(comp, 
#            into = c("Hab", "Cov"), 
#            sep = "(?<=[A-Za-z])(?=[0-9])"
#   ) %>%
#   mutate(Hab=ifelse(Hab=="Ht","H",Hab)) %>%
#   mutate(Cov=as.numeric(Cov)) %>%
#   spread(key=Hab, value=Cov, fill=0)
# 
# transects<-transects %>% left_join(habitats, by="Transect")
#fwrite(transects[,c(1:6,11:18)],"PIRW_transects_habitat.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILL IN MISSING SURVEYS AND CREATE DATAFRAME WITH ALL DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(surveys)

### sum all birds (of age classes etc.) for each survey
simplecounts<-counts %>%
  group_by(LandbirdSurveyID) %>%
  summarise(N_birds=sum(N_birds))

### combine counts with survey and metadata
ALLDAT<-surveys %>% mutate(Effort=as.numeric(End_Time-Start_time),Daytime=hour(Start_time)*60+minute(Start_time),DAY=yday(Date)) %>%
  group_by(Transect) %>%
  arrange(Transect,Date,Start_time) %>%
  mutate(Count=seq_along(Start_time)) %>%
  select(LandbirdSurveyID,Transect,DAY,Count,Daytime,Effort,Wind, Rain, Visibility) %>%
  left_join(simplecounts, by="LandbirdSurveyID") %>%
  mutate(N_birds=ifelse(is.na(N_birds)==T,0,N_birds)) %>%
  ungroup()
dim(ALLDAT)
head(ALLDAT)
ALLDAT %>% filter(Effort<0)


### troubleshoot counts
ALLDAT %>% ungroup() %>% group_by(Transect) %>%
  summarise(N=length(unique(LandbirdSurveyID))) %>%
  arrange(N)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUBSAMPLE TRANSECTS THAT ARE 50 M APART
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### create SF OBJECT

transline<-ALLDAT %>% group_by(Transect) %>%
  summarise(max=max(N_birds)) %>%
  left_join(transects, by="Transect") %>%
  arrange(Transect) %>%
  select(Transect, Long_start,Lat_start,Long_end,Lat_end, max)

# Create list of simple feature geometries (linestrings)
l_sf <- vector("list", nrow(transline))
for (i in seq_along(l_sf)){
  l_sf[[i]] <- st_linestring(matrix(as.numeric(transline[i,2:5]), ncol=2, byrow=T))
}

# Create simple feature geometry by combining dataframe with geometry column
l_sfc <- st_sfc(l_sf, crs = "+proj=longlat +datum=WGS84")
lines_sf <- st_sf('Transect' = transline$Transect, 'geometry' = l_sfc) %>%
  left_join(transline[,c(1,6)], by="Transect")
str(lines_sf)
st_crs(lines_sf)

# Create a 50 m buffer around each transect
trans_buff<- lines_sf %>% st_transform(crs = 3857) %>%
  st_buffer(dist=50) %>%
  rename(Buffer=Transect)

# overlay transects with buffer and filter those transects with >1 buffer

overlaps <- lines_sf %>% st_transform(crs = 3857) %>%
  st_join(trans_buff,
          join = st_intersects, 
          left = TRUE) %>%
  filter(!(Transect==Buffer))

overlapcount <- lines_sf %>% st_transform(crs = 3857) %>%
  st_join(trans_buff,
          join = st_intersects, 
          left = TRUE) %>%
  group_by(Transect) %>%
  summarise(overlaps=length(unique(Buffer))) %>%
  arrange(desc(overlaps))


#### REMOVE TRANSECTS ONE BY ONE AND RECALCULATE UNTIL BUFFER OVER LAP IS 0

exclude<-overlapcount %>% filter(overlaps>4)

for (xl in 1:50){
  red_lines<-lines_sf %>% filter(!(Transect %in% exclude$Transect))
  red_buff<-trans_buff %>% filter(!(Buffer %in% exclude$Transect))

  overlapcount <- red_lines %>% st_transform(crs = 3857) %>%
    st_join(red_buff,
            join = st_intersects, 
            left = TRUE) %>%
    filter(!(Transect==Buffer)) %>%
    group_by(Transect) %>%
    summarise(overlaps=length(unique(Buffer))) %>%
    filter(overlaps>0) %>%
    arrange(desc(overlaps))
  if(dim(overlapcount)[1]>0){exclude<-bind_rows(exclude,overlapcount[1,])}
}

  
### REMOVE THE 25 OVERLAPPING TRANSECTS

ALLDAT<-ALLDAT %>%
  filter(!(Transect %in% exclude$Transect))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATING A MATRIX WITH the count data in separate columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### CAST THE DATA FRAME INTO MATRIX WITH 1 COLUMN PER (COUNT) 
PIRW_y<- ALLDAT %>%
  select(Transect,Count,N_birds) %>%
  spread(key=Count, value=N_birds) %>%
  arrange(Transect)
PIRW_y


### CREATE EQUALLY SIZED OBSERVATION COVARIATES

obscovs<-ALLDAT %>%
  select(Transect,Count,DAY,Effort,Daytime,Effort,Wind,Rain,Visibility) %>%
  mutate(Wind=ifelse(is.na(Wind),"moderate",Wind)) %>%
  arrange(Transect,Count)
head(obscovs)
obscovs$Wind
hist(obscovs$DAY)

### CREATE EQUALLY SIZED SITE COVARIATES

sitecovs<-ALLDAT %>% filter(Count==1) %>%
  left_join(transects, by="Transect") %>% 
  mutate(Length = pmap(list(a = Long_start, 
                              b = Lat_start, 
                              x = Long_end,
                              y = Lat_end), 
                         ~ distRhumb(c(..1, ..2), c(..3, ..4)))) %>%
  mutate(Length=as.numeric(Length)) %>%
  select(Transect,Length,B,G,F,H,P,RA,L,M) %>%
  arrange(Transect)
head(sitecovs)
scale(sitecovs[,4])


### CAST DATA FOR UNMARKED INPUT

PIRW_y<-as.matrix(PIRW_y[,2:dim(PIRW_y)[2]], dimnames=NULL)

### CHECK THAT DATA FRAMES ARE RIGHT SIZE
dim(PIRW_y)*4
dim(sitecovs)*4
dim(obscovs)



##### COMBINE RESPONSE AND OBSERVATION COVARIATES TO UNMARKED FRAME AND STANDARDIZE NUMERIC COVARIATES #######

PIRW_UMF<-unmarkedFramePCount(y=PIRW_y, siteCovs=sitecovs, obsCovs=obscovs)
obsCovs(PIRW_UMF)[,c(3,4,5)] <- scale(obsCovs(PIRW_UMF)[,c(3,4,5)])
siteCovs(PIRW_UMF)[,c(2:10)] <- siteCovs(PIRW_UMF)[,c(2:10)]/100  ##scale(siteCovs(PIRW_UMF)[,c(2:10)]) - switched to easier scale for prediction
summary(PIRW_UMF)
colnames(obsCovs(PIRW_UMF))
colnames(siteCovs(PIRW_UMF))



###################################################################################################
######## ANALYSIS OF DATA #########################################################################
###################################################################################################

### in pcount, first formula is for detection, second formula is for abundance  ####

### FIRST WE TEST FOR THE MOST PARSIMONIOUS MODEL FOR DETECTION
null <- pcount(~1 ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
wind <- pcount(~as.factor(Wind) ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
time <- pcount(~Daytime ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
day <- pcount(~DAY ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
timeday <- pcount(~DAY+Daytime ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
timewind <- pcount(~as.factor(Wind)+Daytime ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
full <- pcount(~DAY+Daytime+as.factor(Wind) ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)

detfl <- fitList(null,wind,time, day,timeday,timewind,full)
detms <- modSel(detfl, nullmod="null")
detms

### DAY AND DAYTIME IS BEST DETECTION MODEL TO BE USED IN MODEL GOVERNING ABUNDANCE
null <- pcount(~DAY+Daytime+as.factor(Wind) ~Length, data= PIRW_UMF, K=20,mixture="P",se=T)
garden <- pcount(~DAY+Daytime+as.factor(Wind) ~G+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
hibiscus <- pcount(~DAY+Daytime+as.factor(Wind) ~H+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
fern <- pcount(~DAY+Daytime+as.factor(Wind) ~F+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
roseapple <- pcount(~DAY+Daytime+as.factor(Wind) ~RA+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
lantana <- pcount(~DAY+Daytime+as.factor(Wind) ~L+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pandanus <- pcount(~DAY+Daytime+as.factor(Wind) ~P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose <- pcount(~DAY+Daytime+as.factor(Wind) ~RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden <- pcount(~DAY+Daytime+as.factor(Wind) ~G+RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden_lan <- pcount(~DAY+Daytime+as.factor(Wind) ~L+G+RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden_lan_fern <- pcount(~DAY+Daytime+as.factor(Wind) ~L+F+G+RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden_lan_hib <- pcount(~DAY+Daytime+as.factor(Wind) ~L+H+G+RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden_lan_hib_fern <- pcount(~DAY+Daytime+as.factor(Wind) ~F+L+H+G+RA+P+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
full <- pcount(~DAY+Daytime+as.factor(Wind) ~F+H+L+G+RA+P+M+Length, data= PIRW_UMF, K=20,mixture="P",se=T)
pan_rose_garden_hib_fern <- pcount(~DAY+Daytime+as.factor(Wind) ~F+H+G+RA+P+M+Length, data= PIRW_UMF, K=20,mixture="P",se=T)


### #################################################################################################
### GOODNESS OF FIT TEST FOR FULL MODEL
### #################################################################################################
### this runs 40 min and yields c-hat = 1.31 and p =0.0002
# GOF<-Nmix.gof.test(full,nsim=5000)

# 
# # GOF from cornell lab of ornitology and the best practise for ebird data
# # (code from: https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html)
# fitstats <- function(full, method = "nonparboot") {
#   observed <- getY(full@data)
#   expected <- fitted(full)
#   resids <- residuals(full, method = "nonparboot")
#   sse <- sum(resids^2, na.rm = TRUE)
#   chisq <- sum((observed - expected)^2 / expected, na.rm = TRUE)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, na.rm = TRUE)
#   out <- c(SSE = sse, Chisq = chisq, freemanTukey = freeTuke)
#   return(out)}
# 
# # calculation with unmarked::parboot
# (pb <- parboot(full, fitstats, nsim = 5000, report = TRUE, 
#                method = "nonparboot")) ## prob of 0.9 is ok
# 
# 
# 
# # Call: parboot(object = full, statistic = fitstats, nsim = 5000, report = TRUE, method = "nonparboot")
# # 
# # Parametric Bootstrap Statistics:
# #   t0 mean(t0 - t_B) StdDev(t0 - t_B) Pr(t_B > t0)
# # SSE          946            203             73.1       0.0046
# # Chisq        502            120             28.9       0.0006
# # freemanTukey 188             36             10.6       0.0008
# # 
# # t_B quantiles:
# #   0% 2.5% 25% 50% 75% 97.5% 100%
# # SSE          519  610 693 741 792   896 1045
# # Chisq        289  330 362 381 400   442  600
# # freemanTukey 113  132 145 152 159   174  194
# # 
# # t0 = Original statistic computed from data
# # t_B = Vector of bootstrap samples
# 
# 
### AIC MODEL TABLE COMPARING THESE MODELS
fl <- fitList(null,garden,fern,hibiscus,roseapple,lantana,pandanus,pan_rose,pan_rose_garden,pan_rose_garden_lan,pan_rose_garden_lan_fern,pan_rose_garden_lan_hib,pan_rose_garden_lan_hib_fern,full,pan_rose_garden_hib_fern)
ms <- modSel(fl, nullmod="null")
ms

fwrite(ms@Full,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLES1.csv")
# fwrite(ms@Full,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLES1.csv")

summary(full)
summaryOD(full)



### #################################################################################################
### SUMMARISE ESTIMATED ABUNDANCE
### #################################################################################################
PIRWRESULT <- predict(full, type='state', newdat=PIRW_UMF, appendData=T, SE=T) %>% filter(!(upper>100)) %>% # remove transect without observations at odd time
  summarise(Abundance=sum(Predicted), lcl=sum(lower), ucl=sum(upper)) %>%
  mutate(Count="Estimate")
PIRWRESULT





### ##############################################
### PLOTTING A GRAPH showing ESTIMATED AND SUMMARISED OBSERVED ABUNDANCE
### ##############################################


data.frame(Abundance=colSums(PIRW_y), lcl=colSums(PIRW_y),ucl=colSums(PIRW_y),Count=c("First","Second","Third","Fourth")) %>%
  bind_rows(PIRWRESULT) %>%
  mutate(Numbers=c("observed","observed","observed","observed","estimated")) %>%

  ggplot(aes(x = Count, y=Abundance, fill=Numbers)) +                      
  geom_bar(stat="identity")+
  geom_errorbar(aes(x=Count,ymin=lcl, ymax=ucl), width=.1) +
  labs(x="", y="Number of Pitcairn Reed Warblers") +
  #scale_x_continuous(limits=c(2008,2023), breaks=seq(2008,2023,1), labels=seq(2008,2023))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=16),
        legend.title = element_text(size=18),
        legend.position=c(0.8,0.88),
        panel.grid.major = element_line(size=.1, color="grey94"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black"))






## #################################################################################################
## EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND USING RANDOM 100 m grid
## #################################################################################################

head(points)
HAB_PTS<- points %>% mutate(Length=1) %>%
#HAB_PTS<- as.data.frame(hab_pts) %>% mutate(Length=1) %>%
  filter(!(SEA==100)) %>%
  filter(!(B==100)) %>%
  mutate(G=G/100,F=F/100,H=H/100,L=L/100,M=M/100,P=P/100,RA=RA/100)
dim(HAB_PTS)  


HAB_PT_ABUND<-predict(full, type='state', newdat=HAB_PTS, appendData=F, SE=T) %>%
  mutate(dens_mean=Predicted/(1+pi*(0.5^2)), dens_lcl=lower/(1+pi*(0.5^2)),dens_ucl=upper/(1+pi*(0.5^2))) %>% ## each transect has a 50m wide dfetection buffer around it
  #summarise(N=sum(Predicted)/(1+pi*(0.5^2)), lcl=sum(lower)/(1+pi*(0.5^2)),ucl=sum(upper)/(1+pi*(0.5^2))) ## each transect is 1 ha (50 m wide)
  summarise(N=sum(dens_mean), lcl=sum(dens_lcl),ucl=sum(dens_ucl)) ## each transect is 1 ha (50 m wide)


HAB_PT_ABUND
# HAB_PT_ABUND*(490/748)



HAB_PT_ABUND100<-predict(full, type='state', newdat=HAB_PTS, appendData=F, SE=T) %>%
  mutate(dens_mean=Predicted/(2+pi), dens_lcl=lower/(2+pi),dens_ucl=upper/(2+pi)) %>% ## each transect has a 50m wide dfetection buffer around it
  #summarise(N=sum(Predicted)/(1+pi*(0.5^2)), lcl=sum(lower)/(1+pi*(0.5^2)),ucl=sum(upper)/(1+pi*(0.5^2))) ## each transect is 1 ha (50 m wide)
  summarise(N=sum(dens_mean), lcl=sum(dens_lcl),ucl=sum(dens_ucl)) ## each transect is 1 ha (50 m wide)


HAB_PT_ABUND100
# HAB_PT_ABUND100*(490/748)


### ADD SENSITIVITY ANALYSIS FOR DIFFERENT PROPORTION OF ADULT BIRDS ###
## calculate ratio for each survey round

### proportion of adult birds
ad_props<-counts %>% filter(!is.na(Age)) %>% left_join(ALLDAT[,1:5], by="LandbirdSurveyID") %>%
  mutate(Age=tolower(Age)) %>%
  mutate(Age=ifelse(Age=="f","fled",Age)) %>%
  group_by(Age,Count) %>%
  summarise(N=sum(N_birds)) %>%
  spread(key=Count, value=N) %>%
  adorn_totals()
ad_props[1,2:5]/ad_props[4,2:5]


### CREATE ABUNDANCE OUTPUT TABLE
ABUND_OUT<-expand.grid(ad_ratio=as.numeric((ad_props[1,2:5]/ad_props[4,2:5])), radius=c(50,100),N=0,lcl=0,ucl=0)
ABUND_OUT[1:4,3:5]<-HAB_PT_ABUND
ABUND_OUT[5:8,3:5]<-HAB_PT_ABUND100

ABUND_OUT %>% mutate(ad_N=N*ad_ratio,ad_lcl=lcl*ad_ratio,ad_ucl=ucl*ad_ratio) %>%
  arrange(radius,ad_N)



## INCLUDE PARAMETER ESTIMATES IN TABLE 2
ABUND_TABLE<-predict(full, type='state', newdat=HAB_PTS, appendData=T, SE=T) %>%
  mutate(dens_mean=Predicted/(1+pi*(0.5^2)), dens_lcl=lower/(1+pi*(0.5^2)),dens_ucl=upper/(1+pi*(0.5^2))) %>%
  select(Transect,P,RA,L,H,G, F,dens_mean, dens_lcl,dens_ucl) %>%
  arrange(desc(dens_mean))

ABUND_TABLE %>% filter(RA==0)
ABUND_TABLE %>% filter(P>0.9)
ABUND_TABLE %>% filter(H>0.5)





### #################################################################################################
### NUMBERS NEEDED FOR MANUSCRIPT
### #################################################################################################

### duration of surveys
summary(ALLDAT$Effort)

### total number of birds
summary(ALLDAT$N_birds)

### study period
summary(surveys$Date)

### proportion of adult birds
counts %>% filter(!is.na(Age)) %>%
  group_by(Age) %>%
  summarise(N=sum(N_birds)) %>%
  adorn_totals()
490/748




### #################################################################################################
### PLOT MAP OF STUDY AREA WITH TRANSECTS
### #################################################################################################

### VERY COMPLICATED DUE TO MISMATCHING CRS between stamen map and sf objects
### found online workaround

## DOWNLOAD MAP FOR BACKGROUND
PITmap <- get_stamenmap(bbox = matrix(c(-130.125,-130.087,-25.082,-25.058),ncol=2,byrow=T),
                          maptype = "terrain",
                          crop = TRUE,
                          zoom = 13)
ggmap(PITmap) + 
  xlab("Longitude")+
  ylab("Latitude")

## CONVERSION FUNCTION TO PLOT OTHER STUFF ON MAP
### very hard to plot sf object over ggmap object
## https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster


ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
base <- ggmap_bbox(PITmap)





### CREATE KML LINES OBJECT TO VISUALISE WHERE PIRW ARE

plotdat<-ALLDAT %>% group_by(Transect) %>%
  summarise(mean=mean(N_birds), max=max(N_birds)) %>%
  left_join(transects, by="Transect") %>%
  arrange(Transect) %>%
  left_join(predict(pan_rose_garden, type='state', newdat=PIRW_UMF, appendData=T, SE=T), by="Transect") %>%
  select(Transect, Long_start,Lat_start,Long_end,Lat_end, Predicted, mean, max)



# Create list of simple feature geometries (linestrings)
l_sf <- vector("list", nrow(plotdat))
for (i in seq_along(l_sf)){
  l_sf[[i]] <- st_linestring(matrix(as.numeric(plotdat[i,2:5]), ncol=2, byrow=T))
}

# Create simple feature geometry by combining dataframe with geometry column
l_sfc <- st_sfc(l_sf, crs = "+proj=longlat +datum=WGS84")
lines_sf <- st_sf('Transect' = plotdat$Transect, 'geometry' = l_sfc) %>%
  left_join(plotdat[,c(1,6:8)], by="Transect") %>%
  left_join(sitecovs[,c(1,4,7,8)], by="Transect") %>% mutate(Type="Transect")
str(lines_sf)
st_crs(lines_sf)

#### EXPORT OUTPUT TO KML

#st_write(lines_sf, "PIRW_transects.kml", layer="PIRW_transects", driver="KML", delete_dsn=T, delete_layer=T)




### create SF object for nests
nests_sf<-st_as_sf(nests,coords = c("Longitude","Latitude")) %>% mutate(Type="Nest")
st_crs(nests_sf)<-st_crs(lines_sf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE DENSITY GRID
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ABUND_MAP<-predict(full, type='state', newdat=HAB_PTS, appendData=T, SE=T) %>%
  mutate(dens_mean=Predicted/(1+pi*(0.5^2)), dens_lcl=lower/(1+pi*(0.5^2)),dens_ucl=upper/(1+pi*(0.5^2))) %>%
  select(Transect,Lat_start,Long_start, dens_mean, dens_lcl,dens_ucl)
range(ABUND_MAP$dens_mean)

export<-ABUND_MAP %>% rename(Point=Transect,Lat=Lat_start,Long=Long_start) %>%
  mutate(Point=str_replace(Point,"P",""))
fwrite(export,"PIRW_predicted_density.csv")


### create SF object for abundance prediction
abund_sf<-st_as_sf(ABUND_MAP,coords = c("Long_start","Lat_start"))
st_crs(abund_sf)<-st_crs(lines_sf)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT TRANSECTS AND NESTS ON A MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### help to fix axis label: https://stackoverflow.com/questions/53918557/fix-legend-title-when-using-superscript-in-more-than-two-lines-r-ggplot2
### NOTE: to remove panel grid from plot we need to colour the lines white - element.blank() does NOT WORK: https://github.com/tidyverse/ggplot2/issues/2071
# ggmap(PITmap) + 
#   coord_sf(crs = st_crs(3857)) + # force it to be 3857
  
ggmap(base) +
  coord_sf(crs = st_crs(3857)) +

  #ggplot() +
  ### ADD SURVEY TRANSECTS
  #geom_sf(data=lines_sf, aes(color = Predicted), lwd=0.8) +
  geom_sf(data= st_transform(abund_sf, 3857), aes(color = dens_mean), alpha=0.5, size=8, shape= 15,
          inherit.aes = F) +
  
  geom_sf(data = st_transform(lines_sf, 3857), color= "grey52", lwd=1.5,    #aes(color = Predicted),
          inherit.aes = F) +

  geom_sf(data = st_transform(nests_sf, 3857), color= "darkblue", size=2,
          inherit.aes = F) +
  
  scale_color_gradient(name = 'Predicted Reed Warbler \n density (ind per ha)', low="white", high="red", guide = "colourbar", limits=c(0.05, 8.1))+
  
  ### ADD PREDICTED ABUNDANCE SCALE
  #scale_colour_gradient(name = "PIRW \n abundance (ind / ha)", low="grey", high="red", guide = "colourbar", limits=c(0, 20))+
  # scale_color_manual("Reed Warbler surveys",
  #                    values = c("Nest" = "indianred",
  #                               "Transect" = "grey52")) +
  #scale_colour_identity("Reed Warbler surveys") +

  ### FORMAT AXES
  xlab("Longitude") +
  ylab("Latitude") +
  
  ## adding scalebar and north arrow
  #north(location="topright", symbol=3, scale=0.2,x.min=min(preddata$x),x.max=max(preddata$x),y.min=min(preddata$y),y.max=max(preddata$y)) +
  # ggsn::scalebar(location = "bottomleft",dist = 1, dist_unit = "km", transform = TRUE, st.dist=0.06,model = "WGS84",
  #                x.min=-130.125,x.max=-130.087,y.min=-25.082,y.max=-25.058) +
  #annotation_north_arrow( height = unit(1, "km"), width = unit(1, "km"), pad_y = unit(0.7, "km")) +
  #annotation_scale(location = 'bl') + 
  
  ### ADJUST FONT SIZE
  theme_classic()+
  theme(panel.background=element_rect(fill="white", colour="black"),
        plot.background = element_rect(fill = "white"),
        axis.text=element_text(size=14, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.background = element_blank(),
        legend.position=c(0.18,0.15),
        axis.title=element_text(size=18), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"), 
        panel.border = element_blank())


ggsave("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\PIRW_density_map.jpg", width=12, height=8, quality=100)


