################################################################################################################
#############  POPULATION SIZE ASSESSMENT FOR PITCAIRN REED WARBLER   ##########################################
################################################################################################################

### based on Montserrat Oriole approach published in 2014 (Oppel et al. 2014, BCI)
### written by steffen.oppel@rspb.org.uk

### updated on 16 Jan 2023 to extrapolate total abundance across island

### updated 14 Feb after modifications to database

######################################################################################
#############  Load required packages       ##########################################
######################################################################################

library(reshape)
library(tidyverse)
library(lubridate)
library(unmarked)
library(geosphere)
library(janitor)
filter<-dplyr::filter
select<-dplyr::select
rename<-dplyr::rename



######################################################################################
#############  Import data from tables in the Access Database        ############
######################################################################################
setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
setwd("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
#setwd("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
load("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
load("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
#load("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
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
# CREATING A MATRIX WITH the count data in separate columns
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

### AIC MODEL TABLE COMPARING THESE MODELS 
fl <- fitList(null,garden,fern,hibiscus,roseapple,lantana,pandanus,pan_rose,pan_rose_garden,pan_rose_garden_lan,pan_rose_garden_lan_fern,pan_rose_garden_lan_hib,pan_rose_garden_lan_hib_fern,full,pan_rose_garden_hib_fern)
ms <- modSel(fl, nullmod="null")
ms

fwrite(ms@Full,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLES1.csv")
# fwrite(ms@Full,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLES1.csv")

summary(full)




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






### #################################################################################################
### EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND
### #################################################################################################
## WE HAVE HABITAT COMPOSITION
## dismissed on 27 Feb 2023 because 1 ha grid cell composition is more realistic - there are few monoculture patches and predicting pure lantana is nonsensical
# str(summary(full))
# hab_area<-fread("Pitcairn_habitat_area.csv")
# sum(hab_area$Area)   ### total island area is 412 ha
# hab_area<-hab_area %>% filter(Hab!="SEA")
# 
# ### CREATE DATA FRAME TO PREDICT DENSITY
# HAB_DAT<-data.frame(Length=1,
#                     G=c(1,0,0,0,0,0,0),
#                     F=c(0,1,0,0,0,0,0),
#                     H=c(0,0,1,0,0,0,0),
#                     L=c(0,0,0,1,0,0,0),
#                     M=c(0,0,0,0,1,0,0),
#                     P=c(0,0,0,0,0,1,0),
#                     RA=c(0,0,0,0,0,0,1),
#                     Hab=hab_area$Hab) %>%
#   left_join(hab_area, by="Hab") 
# 
# ### TRANSECT AREA IS 100m length + circle of 100 m radius = 2*r^2 + pi*r^2
# 
# HAB_DENS<-predict(full, type='state', newdat=HAB_DAT, appendData=F, SE=T) %>%
#   bind_cols(HAB_DAT) %>%
#   mutate(dens_mean=Predicted/(2+pi), dens_lcl=lower/(2+pi),dens_ucl=upper/(2+pi)) %>% ## each transect is 2 ha
#   mutate(N=dens_mean*Area, lcl=dens_lcl*Area,ucl=dens_ucl*Area) %>% ## extrapolate across island area
#   select(Habitat_description,Area, N, lcl, ucl, dens_mean,dens_lcl,dens_ucl)
# 
# # fwrite(HAB_DENS,"PIRW_habitat_specific_densities_100m.csv")
# 
# TABLE2<- HAB_DENS %>%
#     mutate(Density=paste(round(dens_mean,1)," (",round(dens_lcl,1), " - ",round(dens_ucl,1),")", sep="")) %>%
#     mutate(Abundance=paste(round(N,0)," (",round(lcl,0), " - ",round(ucl,0),")", sep="")) %>%
#   select(Habitat_description,Area, Density, Abundance)
# # 
# # fwrite(TABLE2,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE2.csv")
# # fwrite(TABLE2,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE2.csv")
# 
# 
# # ### SUMMARISE ABUNDANCE ACROSS ISLAND
# # 
# # SUMMARY<- HAB_DENS %>% summarise(N=sum(N), lcl=sum(lcl),ucl=sum(ucl))
# # SUMMARY
# # SUMMARY*(490/748)
# # 
# # 
# # 
# ##### ASSUMING A DETECTION DISTANCE OF ONLY 50 M, EXTRPOLATE NUMBERS
# ### TRANSECT AREA IS 100m length + circle of 50 m radius = 2*r^2 + pi*r^2
# 
# HAB_DENS50<-predict(pan_rose_garden_lan_hib_fern, type='state', newdat=HAB_DAT, appendData=F, SE=T) %>%
#   bind_cols(HAB_DAT) %>%
#   mutate(dens_mean=Predicted/(1+pi*(0.5^2)), dens_lcl=lower/(1+pi*(0.5^2)),dens_ucl=upper/(1+pi*(0.5^2))) %>% ## each transect is 1 ha (50 m wide)
#   mutate(N=dens_mean*Area, lcl=dens_lcl*Area,ucl=dens_ucl*Area) %>% ## extrapolate across island area
#   select(Habitat_description,Area, N, lcl, ucl, dens_mean,dens_lcl,dens_ucl)
# 
# SUMMARY50<- HAB_DENS50 %>% summarise(N=sum(N), lcl=sum(lcl),ucl=sum(ucl))
# SUMMARY50
# SUMMARY50*(490/748)
# 
# TABLE2<- HAB_DENS50 %>%
#   mutate(Density50=paste(round(dens_mean,1)," (",round(dens_lcl,1), " - ",round(dens_ucl,1),")", sep="")) %>%
#   #mutate(Abundance=paste(round(N,0)," (",round(lcl,0), " - ",round(ucl,0),")", sep="")) %>%
#   select(Habitat_description,Area, Density50) %>%
#   left_join(TABLE2, by=c("Habitat_description","Area"))
# 
# 
# ## INCLUDE PARAMETER ESTIMATES IN TABLE 2
# out<-summary(full)
# parms<-as.data.frame(out[[1]])
# parms$Hab<-row.names(parms)
# 
# TABLE2<- parms %>% filter(Hab %in% HAB_DAT$Hab) %>%
#   left_join(HAB_DAT, by="Hab") %>%
#   mutate(Parameter=paste(round(Estimate,2)," (� ",round(SE,2), ")", sep="")) %>%
#   select(Habitat_description,Parameter) %>%
#   left_join(TABLE2, by="Habitat_description") %>%
#   select(Habitat_description,Area,Parameter,Density50,Density)
#   
# fwrite(TABLE2,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE1.csv")
# fwrite(TABLE2,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE1.csv")




## #################################################################################################
## ALTERNATIVE 1 TO EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND: USING RANDOM 100 m grid
## #################################################################################################
## EXTRAPOLATION BASED ON COMPOSITION AT REGULAR 100 m grid points
## this results in 2600 birds unless we curtail density to include a 50 m radius around transects to account for bird movement and detcetion range
### TRANSECT AREA IS 100m length + circle of 50 m radius = 2*r^2 + pi*r^2

## updated on 27 Feb 2023 after receiving file from Robbie with habitat composition
## was incorporated into database on 27 Feb 2023 and no longer needs to be read in as separate file
# library(readxl)
# hab_pts<-read_excel("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Maps\\Percentage_habitat_in_grid_squares.xls", sheet="Percentage habitat in grid")
# hab_pts<-read_excel("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Maps\\Percentage_habitat_in_grid_squares.xls", sheet="Percentage habitat in grid")
# names(hab_pts)<-c("Transect","Habitat_description","area","prop")
# unique(hab_pts$Habitat_description)
# hab_pts<-hab_pts %>% select(-area) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Grass Fern","F",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Not_Veg","B",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Water","SEA",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Garden Vegetation","G",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Pandanus","P",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Hibiscus","H",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Mixed","M",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Lantana","L",Habitat_description)) %>%
#   mutate(Habitat_description=if_else(Habitat_description=="Rose apple","RA",Habitat_description)) %>%
#   spread(key=Habitat_description, value=prop, fill=0) #%>%
#   #adorn_totals("col")
# 
# ## check points that are not covered by 100% of habitat (mostly at the edge of the island)
# # hab_pts %>% filter(Total<100) %>%
# #   arrange(Total)
# 
# 
# ## export to database
# export<-hab_pts %>% mutate(Transect=paste("P",Transect,sep=""))
# fwrite(export,"PIRW_hab_points_for_db.csv")

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
HAB_PT_ABUND*(490/748)



HAB_PT_ABUND100<-predict(full, type='state', newdat=HAB_PTS, appendData=F, SE=T) %>%
  mutate(dens_mean=Predicted/(2+pi), dens_lcl=lower/(2+pi),dens_ucl=upper/(2+pi)) %>% ## each transect has a 50m wide dfetection buffer around it
  #summarise(N=sum(Predicted)/(1+pi*(0.5^2)), lcl=sum(lower)/(1+pi*(0.5^2)),ucl=sum(upper)/(1+pi*(0.5^2))) ## each transect is 1 ha (50 m wide)
  summarise(N=sum(dens_mean), lcl=sum(dens_lcl),ucl=sum(dens_ucl)) ## each transect is 1 ha (50 m wide)


HAB_PT_ABUND100
HAB_PT_ABUND100*(490/748)





## INCLUDE PARAMETER ESTIMATES IN TABLE 2
ABUND_TABLE<-predict(full, type='state', newdat=HAB_PTS, appendData=T, SE=T) %>%
  mutate(dens_mean=Predicted/(1+pi*(0.5^2)), dens_lcl=lower/(1+pi*(0.5^2)),dens_ucl=upper/(1+pi*(0.5^2))) %>%
  select(Transect,P,RA,L,H,G, F,dens_mean, dens_lcl,dens_ucl) %>%
  arrange(desc(dens_mean))

ABUND_TABLE %>% filter(RA==0)
ABUND_TABLE %>% filter(P>0.9)
ABUND_TABLE %>% filter(H>0.5)


### #################################################################################################
### ALTERNATIVE 2 TO EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND: randomly multiplying survey transects
### #################################################################################################
### PRELIMINARY EXTRAPOLATION BASED ONLY ON SAMPLED TRANSECTS
### discarded on 14 Feb after habitat map became available
# amazingly this resulted in very similar abundance across the island as the habitat map
# 
# ISLAND_AREA<-412   ### total island area is 412 ha
# 
# 
# OBS_DENS<-predict(pan_rose_garden, type='state', newdat=PIRW_UMF, appendData=F, SE=T) %>%
#   bind_cols(sitecovs) %>%
#   mutate(dens_mean=Predicted/((Length*200)/10000), dens_lcl=lower/((Length*200)/10000),dens_ucl=upper/((Length*200)/10000)) %>%
#   group_by(P,RA,G) %>%
#   summarise(dens_mean=mean(dens_mean), dens_lcl=mean(dens_lcl),dens_ucl=mean(dens_ucl))
#   
#   
# ### HAB COMP MAP ###
# # assuming naively that we have 100ha each of G, RA, and P and 100 ha of mix and 12 ha of 0 
# scale(sitecovs[,4]) ### get the scaling factor for G
# scale(sitecovs[,7]) ### get the scaling factor for P
# scale(sitecovs[,8]) ### get the scaling factor for RA
# hab_points<-points %>%
#   bind_cols(OBS_DENS[sample(1:22,412, replace=T),1:3]) %>% ### takes RANDOM SAMPLE of existing habitat composition
#   mutate(Length=100) %>%
#   #mutate(G=scale(G,center=25.25,scale=30.46255), RA=scale(RA,center=20.5,scale=32.64053),P=scale(P,center=4.5,scale=13.93528)) ### scale for prediction - but still resulted in nonsense
#   #predict(pan_rose_garden, type='state', newdat=hab_points, appendData=F, SE=T)   ### DID NOT WORK 
#   left_join(OBS_DENS, by=c('P','RA','G'))
# 
# 
# ### SUMMARISE ABUNDANCE ACROSS ISLAND
# SUMMARY<- hab_points %>% summarise(mean=sum(dens_mean), lcl=sum(dens_lcl),ucl=sum(dens_ucl))
# SUMMARY
# SUMMARY*(490/748)






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


library(maptools)
library(sf)
library(ggmap)
library(ggsn)
library(ggspatial)

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
  
  scale_color_gradient(name = 'Predicted Reed Warbler \n density (ind per ha)', low="white", high="red", guide = "colourbar", limits=c(0.1, 8.1))+
  
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


