################################################################################################################
#############  PITCAIRN REED WARBLER NEST SURVIVAL ANALYSIS   ##########################################
################################################################################################################

### based on Montserrat Oriole approach published in 2014 (Oppel et al. 2012, Ibis)
### written by steffen.oppel@rspb.org.uk
### updated 14 Feb 2023



######################################################################################
#############  Load required packages       ##########################################
######################################################################################

library(reshape)
library(tidyverse)
library(lubridate)
library(geosphere)
library(janitor)
filter<-dplyr::filter
select<-dplyr::select
rename<-dplyr::rename



######################################################################################
#############  Import data from tables in the Access Database        ############
######################################################################################
setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
#setwd("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA")
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\Methods\\Scripts\\RODBC_data_import.r")), wait = TRUE, invisible = FALSE)
load("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
#load("C:\\STEFFEN\\RSPB\\UKOT\\Pitcairn\\Science\\DATA\\PIRW_data.RData")
ls()





# ######################################################################################
# #############  CALCULATE EXPOSURE DAYS FOR EACH NEST          ###########################
# ######################################################################################
head(nests)
dim(nests)
nests<-nests %>% filter(Completed==1) %>% ## remove two nests that were incomplete when Nik left island in late Jan 2023
  select(NestID, StageFound,DateFound,DateLastAlive,DateLastChecked,SUCCESS)
head(nests)
dim(nests)

### ADD THE STOPDATE AS THE MID POINT BETWEEN LAST ALIVE AND LAST CHECK FOR FAILED NESTS
nests<- nests %>%
  mutate(StopDate=if_else(SUCCESS==1,DateLastChecked,DateLastAlive+0.5*(DateLastChecked-DateLastAlive))) %>%
  mutate(exposure=as.numeric(StopDate-DateFound)/(3600*24))
           




# ######################################################################################
# #############  CALCULATE MAYFIELD NEST SURVIVAL          ###########################
# ######################################################################################

### CALCULATE MAX N NEST DAYS FOR SUCCESSFUL NESTS
N_DAYS<-nests %>% filter(SUCCESS==1) %>% filter(StageFound %in% c("TERR","INCU")) %>%
  mutate(duration=as.numeric(DateLastChecked-DateFound)) %>%
  filter(!NestID %in% c(33,34)) %>%  ### remove an early force-fledge 33 and a late INCU find 34
  summarise(mean=mean(duration))



### CALCULATE MAYFIELD NEST SUCCESS ####
exposure<-sum(nests$exposure)
fate<-dim(nests)[1]-sum(nests$SUCCESS)
Mayfield_dsr<-1-(fate/exposure)
Mayfield_nest_success<-Mayfield_dsr^N_DAYS
Mayfield_nest_success




# ######################################################################################
# #############  STATISTICS FOR THE TEXT          ###########################
# ######################################################################################

dim(nests)
dim(nests[nests$SUCCESS==1,])

table(nests$StageFound)


### PRODUCTIVITY ####
lastvisits<-visits %>% select(NestID,Date,Stage,Status,Content) %>%
  filter(NestID %in% nests$NestID[nests$SUCCESS==1]) %>%
  arrange(NestID,Date) %>%
  group_by(NestID,Stage,Status) %>%
  summarise(last=max(Date), N=last(Content))

prodnests<-lastvisits %>% filter(Stage!="INCU") %>% select(-Status,-last) %>% 
  group_by(NestID) %>% #print(n=70)
  spread(key=Stage,value=N) %>% 
  mutate(FLED=ifelse(FLED==0,CHIC,FLED)) %>% 
  mutate(FLED=ifelse(is.na(FLED),CHIC,FLED)) %>% 
  filter(!is.na(FLED)) %>%
  ungroup() %>%
  left_join(nests, by="NestID")

dim(prodnests)
summary(prodnests)
  
mean(prodnests$FLED) * Mayfield_nest_success
sd(prodnests$FLED) * Mayfield_nest_success


