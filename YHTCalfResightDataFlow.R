
library(lubridate)
library(dplyr)
library(R2jags)
library(mcmcplots)
library(tidyr)
source("C:/Users/Hans Martin/Documents/GitHub/YHTElkSurvivalDataFlow/NowakSurvFunctions.R")

Calf_Obs<-read_csv("C:/Users/Hans Martin/Documents/GitHub/YHTElkSurvivalDataFlow/Data/ELK_CALF_OBSERVATIONS.csv")

Elk_ID_Key <- read_csv("C:/Users/Hans Martin/Box Sync/YaHaTindaDatabases/ElkIDKEY_Alias_Cows_Calf_Bulls.csv")

Immob_Data <- read.csv(file = "C:/Users/Hans Martin/Documents/GitHub/YHTElkSurvivalDataFlow/Data/Immobilization Data.csv", header = T, stringsAsFactors = F)

IMMOB_ID_TABLE <- read.csv("C:/Users/Hans Martin/Box Sync/YaHaTindaDatabases/ElkIDEarTagAnimalIDKEY.csv") %>% select(Immob.Number, Species.x, Date.x, Animal.IDHans)

Immob <- Immob_Data %>%
  left_join(IMMOB_ID_TABLE, by = c("Immob.Number" = "Immob.Number"), keep = T) %>%
  mutate(ElkEarTagID = stringr::str_trim(toupper(Animal.IDHans), side = c("both"))) %>%
  select(-Animal.IDHans) %>%
  mutate(ElkEarTagID = stringr::str_replace_all(ElkEarTagID, " ", "")) %>%
  mutate(ElkEarTagID1 = ifelse(is.na(ElkEarTagID), Animal.Name, ElkEarTagID)) %>%
  left_join(Elk_ID_Key, by = c("ElkEarTagID1" = "Alias")) %>%
  mutate(
    elkid = ifelse(is.na(Animal.IDHans),
                   ElkEarTagID1,
                   Animal.IDHans
    ),
    Immob_Date = as.Date(Date, format = "%m/%d/%Y"),
    Year=as.integer(format(Immob_Date,"%Y"))
  ) %>% 
  select(elkid,Immob_Date,Year,Pregnant,BCS,Rump,Loin)

MigratoryStatus <- read.csv("C:/Users/Hans Martin/Box Sync/YaHaTindaDatabases/MigrationData/YHTMigrationClassification_2002_2018.csv", stringsAsFactors = F)
MigratoryStatus <- MigratoryStatus %>% 
  select(elkid, year, MigratorySegmentofPop, MigrationRoute) %>% 
  unite(col=elkid_year,c("elkid","year"),sep="_",remove = F)

month_lookup_table<-tibble::tibble(index_month=c(1:11),named_month=c("june","july","aug","sept","oct","nov","dec","jan","feb","mar","april"),numeric_month=c(6,7,8,9,10,11,12,1,2,3,4))

Calf_Obs_Data<-Calf_Obs %>%
  mutate(ElkEarTagID = stringr::str_trim(toupper(`ELK ID`), side = c("both")),
         ElkEarTagID = stringr::str_replace_all(ElkEarTagID, " ", "")) %>%
  left_join(Elk_ID_Key, by = c("ElkEarTagID" = "Alias")) %>%
  mutate(
    elkid = ifelse(is.na(Animal.IDHans),
                   ElkEarTagID,
                   Animal.IDHans),
    Date=as.Date(DATE,"%m/%d/%Y"),
    Year=as.integer(format(Date,"%Y")),
    Bio_year = case_when(
      as.integer(format(Date,"%m"))>5 ~ as.integer(Year),
      as.integer(format(Date,"%m"))<6 ~ as.integer(Year-1)),
    Month= as.integer(format(Date,"%m")),
    Day=as.integer(format(Date,"%m"))
    
    
) %>% 
  left_join(month_lookup_table,by=c("Month"="numeric_month")) %>% 
  left_join(MigratoryStatus, by=c("elkid"="elkid","Bio_year"="year")) %>% 
  left_join(Immob,c("elkid"="elkid","Bio_year"="Year")) %>%
  unite(col=elkid_year,c("elkid","Bio_year"),sep="_",remove = F) %>%
  ##########
#FILTERS
##########
  filter(Month!=5) %>% 
  filter(Bio_year %in% c(2002,2003,2004,2017,2018)) %>%
##########
  group_by(elkid_year) %>% 
  mutate(
    Col=surv_build_interval(
      Date,
      start =as.Date(paste("6/1/",Bio_year, sep=""),"%m/%d/%Y"),
      time_int = "week",
      increment=2),
    ID=elkid_year,
    State1=`CALF PRESENT?`,
    MigrationSegment=replace(MigratorySegmentofPop,MigratorySegmentofPop=="unk",NA),
    Index_migration=as.integer(factor(MigrationSegment,levels=c("resident","east","west")))
  ) %>% 
  filter(!is.na(Index_migration))

EH_Calf_Obs<-Calf_Obs_Data%>%
  group_by(ID,Col) %>% 
  summarize(State=if(all(is.na(State1))){
    NA
  }else{
    max(State1, na.rm = T)
  }
  ) %>% 
  ungroup() %>%   
  surv_build_eh(vars=State,F) %>% 
  mutate('0'=1)#%>%  
  #select(-ID)
##################################
#################################
#STOPED HERE
EH_Month<-Calf_Obs_Data %>%
  
  #mutate(Index_month=as.integer(factor(Month))) %>% 
  group_by(ID,Col) %>%
  
  summarise(index_month=max(index_month))%>% 
  surv_build_eh(vars=index_month,F) %>% 
  summarize_all(surv_smrz_safe
  ) %>% 
  ungroup() %>%  
  select(-1,-2) %>% 
  as.numeric(.)
#month_lookup_table<-tibble::tibble(indexmonth=c(1:11),month=c("june","july","aug","sept","oct","nov","dec","jan","feb","mar","april"),numeric_month=c(6,7,8,9,10,11,12,1,2,3,4))

EH_Year<-  EH_Calf_Obs %>% 
  select(ID) %>% 
  mutate(
    Year=as.integer(substrRight(ID,4)),
    Index_year=as.integer(as.factor(Year))
    )

EH_Migr<-  EH_Calf_Obs %>% 
  mutate(
    Bio_year=as.integer(substrRight(ID,4))
    ) %>% 
  left_join(MigratoryStatus, by=c("ID"="elkid_year")) %>% 
  mutate( 
    MigrationSegment=replace(MigratorySegmentofPop,MigratorySegmentofPop=="unk",NA),
    Index_migration=as.integer(factor(MigrationSegment,levels=c("resident","east","west"))),
    Index_mig_res=replace(Index_migration,Index_migration==3,2)
    ) %>% 
  select(ID,Index_mig_res,Index_migration)

first_last <- Calf_Obs_Data%>%
  group_by(ID) %>% 
  summarise(
    f = 1,
    l = max(Col, na.rm = T) + 1
   
  )

YHT.Calf.Obs.Data<-list(
  eh=EH_Calf_Obs %>% select(-ID),
  f=first_last %>% pull(f),
  year=EH_Year %>% pull(Index_year),
  month=EH_Month,
  nmonth=11,
  migr=EH_Migr %>% pull(Index_migration),
  nmigr=3,
  nyear=max(EH_Year$Index_year),
  nind=nrow(EH_Calf_Obs),
  nocc=max(Calf_Obs_Data$Col),
  migr2=EH_Migr %>% pull(Index_mig_res),
  nmigr2=2
  )
  
