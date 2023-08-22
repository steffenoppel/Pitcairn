################################################################################################################
#############  BEHAVIOUR ASSESSMENT FOR PITCAIRN REED WARBLER   ##########################################
################################################################################################################

### investigating whether ground foraging behaviour changes with bait on ground
### written by steffen.oppel@rspb.org.uk
### updated 15 Feb 2023 after modifications to database


######################################################################################
#############  Load required packages       ##########################################
######################################################################################

library(reshape)
library(tidyverse)
library(lubridate)
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
# TRANSCRIBE THE BEHAVIOUR INFORMATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(behav)
tail(behavstints)

bouts<-behavstints %>% mutate(duration=as.numeric(EndTime-StartTime)) %>%
  select(ObsBoutID,duration,MaxNumber,BOG, colour)

bouts<-behav %>% group_by(ObsBoutID) %>%
  summarise(N_tot=sum(N_birds)) %>%
  left_join(bouts, by="ObsBoutID") %>%
  mutate(pot=((duration/20)*MaxNumber)+1) %>%
  mutate(coverage=N_tot/pot)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISING PROPORTION OF ALL BEHAVIOURS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BEHAV_SUMMARY<-behav %>% left_join(bouts, by="ObsBoutID") %>%
  mutate(prop=N_birds/N_tot, abs_prop=N_birds/pot) %>%
  group_by(BOG,colour,BehavCode) %>%
  summarise(mean_prop=mean(prop),lcl=quantile(prop, probs=0.025), ucl=quantile(prop,probs=0.975),
            mean_prop_abs=mean(abs_prop),lcl_abs=quantile(abs_prop, probs=0.025), ucl_abs=quantile(abs_prop,probs=0.975)) %>%
  mutate(colour=ifelse(BOG==0,"none",colour)) %>%
  left_join(behavcode, by="BehavCode")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISING PROPORTION OF FORAGING on SUBSTRATE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FORAGE_SUMMARY<-behav %>% 
  left_join(behavcode, by="BehavCode") %>%
  filter(Behaviour=="foraging") %>%
  group_by(Perch) %>%
  summarise(N=sum(N_birds)) %>%
  ungroup() %>%
  mutate(prop=round(N/sum(N)*100, 2))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISING PROPORTION OF TIME SPENT ON GROUND
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GROUND_SUMMARY<-behav %>% left_join(bouts, by="ObsBoutID") %>%
  mutate(prop=N_birds/N_tot, abs_prop=N_birds/pot) %>%
  mutate(colour=ifelse(BOG==0,"none",colour)) %>%
  left_join(behavcode, by="BehavCode") %>%
  group_by(Perch,BOG) %>%
  summarise(N=sum(N_birds)) %>%
  ungroup() %>%
  group_by(BOG) %>%
  mutate(prop=round(N/sum(N)*100, 2))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISING PROPORTION OF TIME SPENT ON GROUND
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FORAGE_SUMMARY<-behav %>%
  left_join(behavcode, by="BehavCode") %>%
  group_by(Behaviour) %>%
  summarise(N=sum(N_birds)) %>%
  ungroup() %>%
  mutate(prop=round(N/sum(N)*100, 2))




### ##############################################
### PLOTTING A GRAPH showing BEHAVIOUR PROPS
### ##############################################


ggplot(data=BEHAV_SUMMARY,aes(x = colour, y=mean_prop, fill=colour)) +                      
  geom_bar(stat="identity")+
  geom_errorbar(aes(x=colour,ymin=lcl, ymax=ucl, colour=colour), width=.1) +
  labs(x="Presence of bait pellets", y="Frequency of behaviour") +
  facet_grid(Perch~Behaviour) +
  scale_colour_manual(name="Bait", values=c("cornflowerblue","goldenrod","grey50"))+
  scale_fill_manual(name="Bait", values=c("cornflowerblue","goldenrod","grey50"))+

  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18),
        #legend.text=element_text(size=16),
        #legend.title = element_text(size=18),
        legend.position = "none",
        strip.text=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        #legend.position=c(0.1,0.2),
        panel.grid.major = element_line(size=.1, color="grey94"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black"))

ggsave("PIRW_behaviour_bait.jpg", width=9, height=9)







### #################################################################################################
### EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND
### #################################################################################################
### WE HAVE HABITAT COMPOSITION 
summary(pan_rose_garden_lan_hib_fern)
hab_area<-fread("Pitcairn_habitat_area.csv")
sum(hab_area$Area)   ### total island area is 412 ha
hab_area<-hab_area %>% filter(Hab!="SEA")

### CREATE DATA FRAME TO PREDICT DENSITY
HAB_DAT<-data.frame(Length=1,
                    G=c(1,0,0,0,0.2,0,0),
                    F=c(0,1,0,0,0.2,0,0),
                    H=c(0,0,1,0,0.2,0,0),
                    L=c(0,0,0,1,0.2,0,0),
                    P=c(0,0,0,0,0.2,1,0),
                    RA=c(0,0,0,0,0,0,1),
                    Hab=hab_area$Hab) %>%
  left_join(hab_area, by="Hab")

HAB_DENS<-predict(pan_rose_garden_lan_hib_fern, type='state', newdat=HAB_DAT, appendData=F, SE=T) %>%
  bind_cols(HAB_DAT) %>%
  mutate(dens_mean=Predicted/2, dens_lcl=lower/2,dens_ucl=upper/2) %>% ## each transect is 2 ha
  mutate(N=dens_mean*Area, lcl=dens_lcl*Area,ucl=dens_ucl*Area) %>% ## extrapolate across island area
  select(Habitat_description,Area, N, lcl, ucl, dens_mean,dens_lcl,dens_ucl)

fwrite(HAB_DENS,"PIRW_habitat_specific_densities_100m.csv")

TABLE2<- HAB_DENS %>%
    mutate(Density=paste(round(dens_mean,1)," (",round(dens_lcl,1), " - ",round(dens_ucl,1),")", sep="")) %>%
    mutate(Abundance=paste(round(N,0)," (",round(lcl,0), " - ",round(ucl,0),")", sep="")) %>%
  select(Habitat_description,Area, Density, Abundance)

fwrite(TABLE2,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE2.csv")
fwrite(TABLE2,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\PIRW\\TABLE2.csv")


### SUMMARISE ABUNDANCE ACROSS ISLAND

SUMMARY<- HAB_DENS %>% summarise(N=sum(N), lcl=sum(lcl),ucl=sum(ucl))
SUMMARY
SUMMARY*(490/748)







## #################################################################################################
## ALTERNASTIVE 1 TO EXTRAPOLATE TOTAL ABUNDANCE ACROSS ISLAND: USING RANDOM 100 m grid
## #################################################################################################
## EXTRAPOLATION BASED ON COMPOSITION AT REGULAR 100 m grid points
## this results in 2600 birds

head(points)
HAB_PTS<- points %>% mutate(Length=1) %>%
  filter(!(SEA==100)) %>%
  filter(!(B==100)) %>%
  mutate(G=G/100,F=F/100,H=H/100,L=L/100,M=M/100,P=P/100,RA=RA/100)
dim(HAB_PTS)  


HAB_PT_ABUND<-predict(pan_rose_garden_lan_hib_fern, type='state', newdat=HAB_PTS, appendData=F, SE=T) %>%
  summarise(N=sum(Predicted), lcl=sum(lower),ucl=sum(upper)) ## each transect is 1 ha (50 m wide)

HAB_PT_ABUND
HAB_PT_ABUND*(490/748)











### #################################################################################################
### PLOT MAP OF STUDY AREA WITH TRANSECTS
### #################################################################################################

### VERY COMPLICATED DUE TO MISMATCHING CRS between stamen map and sf objects
### found online workaround

library(ggmap)

## DOWNLOAD MAP FOR BACKGROUND
PITmap <- get_stamenmap(bbox = matrix(c(-130.125,-130.087,-25.082,-25.058),ncol=2,byrow=T),
                          maptype = "terrain",
                          crop = TRUE,
                          zoom = 13)
ggmap(PITmap) + 
  xlab("Longitude")+
  ylab("Latitude")


### create SF object for obs_bouts
nests_sf<-st_as_sf(nests,coords = c("Longitude","Latitude")) %>% mutate(Type="Nest")
st_crs(nests_sf)<-st_crs(lines_sf)

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
  geom_sf(data = st_transform(lines_sf, 3857), aes(color = Type), lwd=1.5,    #aes(color = Predicted), 
          inherit.aes = F) +

  geom_sf(data = st_transform(nests_sf, 3857), aes(color = Type), size=2, 
          inherit.aes = F) +
  
  ### ADD PREDICTED ABUNDANCE SCALE
  #scale_colour_gradient(name = "PIRW \n abundance (ind / ha)", low="grey", high="red", guide = "colourbar", limits=c(0, 20))+
  scale_color_manual("Reed Warbler surveys",
                     values = c("Nest" = "indianred",
                                "Transect" = "darkgreen")) +
  #scale_colour_identity("Reed Warbler surveys") +

  ### FORMAT AXES
  xlab("Longitude") +
  ylab("Latitude") +
  
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


ggsave("PIRW_survey_map.jpg", width=10, height=6, quality=100)


