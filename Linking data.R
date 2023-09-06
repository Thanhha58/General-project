require(dplyr)
require(readxl)
require(AMR)
require(stringr)
require(tibble)
require(plyr)
require(tidyr)
require(reshape2)
require(purrr)
library(cleaner)
library(arsenal)
library(knitr)
library(ggplot2)
library(lubridate)
library(stringi)
library(stringdist)

setwd("~/Hospital data")

#duplication function
dedup <-  function(x,window=30){
  x <- x[order(x$SPEC_DATE),]
  x$ord <- 1:nrow(x)
  # create ord data frame with 2 repeated columns for order 
  ord <- tidyr::crossing(x$ord,x$ord) %>% as.data.frame
  names(ord) <- c("order1","order2")
  # creat tmp dataframe with 2 repeated columns for date
  tmp <- tidyr::expand_grid(x[,"SPEC_DATE"],x[,"SPEC_DATE"], .name_repair = "minimal") %>% as.data.frame
  names(tmp) <- c("specdate1","specdate2")
  tmp <- cbind(tmp,ord)
  tmp <- tmp %>% filter(order2 > order1)
  tmp$datediffer <- as.Date(tmp$specdate2,"%Y-%m-%d") - as.Date(tmp$specdate1,"%Y-%m-%d")
  #tmp$datediffer <- tmp$specdate2 - tmp$specdate1
  tmp_filtered <- tmp %>% filter(datediffer > window) 
  
  if(nrow(tmp_filtered)==0) x <- x[1,]
  
  if(nrow(tmp_filtered)>0){
    
    tmp_filtered$ord <- 1:nrow(tmp_filtered)
    tmp1 <- tmp_filtered
    keep <- c(1)
    while (TRUE){
      tmp1 <- tmp1[tmp1$order1 >= tmp_filtered$order2[keep[length(keep)]],]
      if(nrow(tmp1)>0) keep <- c(keep,tmp1$ord[1])
      if(nrow(tmp1)==0) break
      
    } 
    #keep <- keep[!is.na(keep)] 
    x <- x %>% filter(ord %in% unique(c(tmp_filtered$order1[keep], tmp_filtered$order2[keep]) ))
  }
  x <- x %>% select(-ord)
  
}



########------DTH data-----------########
##Loading admin data (year from 2014-2020)
admin_DTH<-readRDS("DTH_2014_2021_admin_with_name.rds") %>% 
  mutate(year.birth=as.numeric(ifelse(stringr::str_detect(year.birth, "/"), NA, year.birth))) %>% 
  mutate(gender=case_when(gender=="Female"~"f",
                             gender=="Male"~"m")) %>% 
  mutate(date.admission=format(as.Date(date.admission), "%Y-%m-%d")) %>%
  mutate(id.patient=as.character(id.patient)) %>% 
  mutate(name_initial_admin = gsub("[^A-Z]+", "", str_to_title(name)))

#Deduplication for micro data (year from 2014-2021)
micro_DTH <- readRDS("DTH_interpret_all_final_COMPLETE.Rds") %>%
  mutate(name_initial= gsub("[^A-Z]+", "", str_to_title(stri_trans_general(str_to_title(paste(LAST_NAME, FIRST_NAME, sep=" ")) , id = "Latin-ASCII")))) %>% 
  mutate(SEX = case_when(SEX=="1"~"f")) %>% 
  mutate(DATE_BIRTH=as.numeric(substr(DATE_BIRTH, 1, 4))) %>% 
  mutate(SPEC_DATE=format(as.Date(SPEC_DATE), "%Y-%m-%d")) %>% 
  mutate(STT=seq.int(nrow(.)))


#No NA for patient ID, specimen, ORG
micro_DTH$group <- paste(micro_DTH$name_initial, micro_DTH$PATIENT_ID, micro_DTH$ORGANISM, micro_DTH$SPECIMEN, sep = "-")
#micro_DTH$SPEC_DATE <- as.Date(micro_DTH$SPEC_DATE,"%Y-%m-%d")
#micro_DTH %>% filter(!is.na(PATIENT_ID)) %>% select(SPEC_DATE) %>% summary()  #ID patient starts from 2020-04-01


dedup_micro_DTH <- micro_DTH[0,]

patientOnlyOneIsolate <- micro_DTH %>% filter(group %in% (table(micro_DTH$group)[table(micro_DTH$group)==1] %>% names()))
patientMoreThanOneIsolate <- micro_DTH %>% filter(group %in% (table(micro_DTH$group)[table(micro_DTH$group)>1] %>% names()))

for (isolatesgroup in unique(patientMoreThanOneIsolate$group)){
  isolatesdf <- subset(x = micro_DTH,group==isolatesgroup)
  dedup_micro_DTH <- rbind(dedup_micro_DTH,dedup(isolatesdf))
}

#Micro_DTH_dedup_final
micro_DTH_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_DTH) 

#Merging by ID
merged_ID_DTH <- full_join(admin_DTH, Micro_DTH_dedup_final, by = c("id.patient" = "PATIENT_ID"))
sum(is.na(merged_ID_DTH$STT)) #91977

#data merged successful with same patient ID
same_ID_record <- merged_ID_DTH %>% filter(!is.na(STT) & !is.na(id.patient)) %>% 
  distinct(id.patient, gender, year.birth, DATE_BIRTH, date.admission, SPEC_DATE, WARD, SPECIMEN, .keep_all = TRUE) %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) 

list_sameID <- unique(same_ID_record$id.patient)


##Subset datasets
#Admin data
sub_admin_DTH<-admin_DTH %>% subset(!(admin_DTH$id.patient %in% list_sameID)) 
sub_micro_DTH<-micro_DTH %>% subset(!(micro_DTH$PATIENT_ID %in% list_sameID)) 

#Merging by sex, birth, name initial
sub_DTH_all<-full_join(sub_admin_DTH, sub_micro_DTH, by=c("year.birth"="DATE_BIRTH", "name_initial_admin"="name_initial"))

#0 records => choosing only same_ID_record file
sub_DTH_all_merge<-sub_DTH_all %>% 
  distinct(name, name_initial_admin, year.birth, WARD, SPECIMEN, .keep_all = TRUE) %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60)


janitor::compare_df_cols(same_ID_record, sub_DTH_all_merge)
sub_DTH_all_merge$PATIENT_ID<-NULL
same_ID_record <- same_ID_record %>% select(-c("DATE_BIRTH", "group", "name_initial"))


Linkdata<-rbind(same_ID_record, sub_DTH_all_merge)
saveRDS(Linkdata, "Linkdata_DTH_all_0307.Rds")



####------------HTD dataset---------#######
##Loading admin data (doesnt have name) (id.mr 0NA)
admin_HTD<-readRDS("HTD_2014_2021_admin_2023-05-16.rds") %>% 
  mutate(date.admission=format(as.Date(date.admission), "%Y-%m-%d")) %>% 
  subset(date.admission>="2016-12-22") %>% 
  mutate(gender = case_when (gender == "Female" ~ "F",
                             gender == "Male" ~ "M"))

unique(format(as.Date(admin_HTD$date.admission), "%Y")) #2016-2021

#Micro data 
micro_HTD <- readRDS("HTD_interpret_all_final_COMPLETE.Rds") %>% 
  mutate(SPEC_DATE=format(as.Date(substr(SPEC_DATE, 1,10)), "%Y-%m-%d")) %>% 
  mutate(name_initial=str_to_title(stri_trans_general(LAST_NAME, id = "Latin-ASCII"))) %>% 
  mutate(name_initial=gsub("[^A-Z]+", "", name_initial)) %>% 
  #mutate (SEX = case_when(SEX=="?"~ NA)) %>% 
  #mutate(DATE_BIRTH=case_when(DATE_BIRTH=="0"~ NA)) %>% 
  mutate(STT=seq.int(nrow(.)))

unique(format(as.Date(micro_HTD$SPEC_DATE), "%Y"))  #2017-2021

#56233 NA for organism code, 0NA specimen code, 392NA for patient id
micro_HTD$group <- paste(micro_HTD$PATIENT_ID,micro_HTD$ORGANISM_CODE, micro_HTD$SPECIMEN_CODE, sep = "-")
micro_HTD$SPEC_DATE <- as.Date(micro_HTD$SPEC_DATE,"%Y-%m-%d")

dedup_micro_HTD <- micro_HTD[0,]

patientOnlyOneIsolate <- micro_HTD %>% filter(group %in% (table(micro_HTD$group)[table(micro_HTD$group)==1] %>% names()))
patientMoreThanOneIsolate <- micro_HTD %>% filter(group %in% (table(micro_HTD$group)[table(micro_HTD$group)>1] %>% names()))

for (isolatesgroup in unique(patientMoreThanOneIsolate$group)){
  isolatesdf <- subset(x = micro_HTD,group==isolatesgroup)
  dedup_micro_HTD <- rbind(dedup_micro_HTD,dedup(isolatesdf))
}


micro_HTD_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_HTD)


 
#Merging by ID

merged_ID_HTD <- full_join(admin_HTD, micro_HTD_dedup_all, by = c("id.mr" = "PATIENT_ID"))


#data merged successful with same patient ID
same_ID_record <- merged_ID_HTD %>% filter(!is.na(STT) & !is.na(id.mr)) %>% 
  distinct(id.mr, gender, year.birth, 
           date.admission, SPEC_DATE, WARD, SPECIMEN_CODE, .keep_all = TRUE) %>%
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  mutate(SPEC_DATE=format(as.Date(SPEC_DATE), "%Y-%m-%d"))


#subset data
list_sameID <- unique(same_ID_record$id.patient)


##Subset datasets
#Admin data
sub_admin_HTD<-admin_HTD %>% subset(!(admin_HTD$id.mr %in% list_sameID))
sub_admin_HTD$year.birth<- as.numeric(admin_HTD$year.birth)

unique(format(as.Date(sub_admin_HTD$date.admission), "%Y")) 
admin_2017 <- sub_admin_HTD %>%  mutate(date.admission = format(as.Date(sub_admin_HTD$date.admission), "%Y-%m-%d")) %>%  
  filter(date.admission <="2017-12-23")
  
admin_2018 <- sub_admin_HTD %>%  mutate(date.admission = format(as.Date(sub_admin_HTD$date.admission), "%Y-%m-%d")) %>%  
  filter(date.admission > "2017-12-23" & date.admission <="2018-12-23")

admin_2019 <- sub_admin_HTD %>%  mutate(date.admission = format(as.Date(sub_admin_HTD$date.admission), "%Y-%m-%d")) %>%  
  filter(date.admission > "2018-12-23" & date.admission <="2019-12-23")

admin_2020 <- sub_admin_HTD %>%  mutate(date.admission = format(as.Date(sub_admin_HTD$date.admission), "%Y-%m-%d")) %>%  
  filter(date.admission > "2019-12-23" & date.admission <="2020-12-23")

admin_2021 <- sub_admin_HTD %>%  mutate(date.admission = format(as.Date(sub_admin_HTD$date.admission), "%Y-%m-%d")) %>%  
  filter(date.admission > "2020-12-23" & date.admission <="2021-12-31")  #update 2021-05

#Micro data
sub_micro_HTD<-micro_HTD_dedup_all %>% subset(!(micro_HTD_dedup_all$PATIENT_ID %in% list_sameID)) 
sub_micro_HTD$DATE_BIRTH<-as.numeric(micro_HTD_dedup_all$DATE_BIRTH)
unique(format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y")) 

micro_2017 <- sub_micro_HTD %>%  mutate(SPEC_DATE = format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y-%m-%d")) %>%  
  filter(SPEC_DATE <="2017-12-31")

micro_2018 <- sub_micro_HTD %>%  mutate(SPEC_DATE = format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y-%m-%d")) %>%  
  filter(SPEC_DATE > "2017-12-31" & SPEC_DATE <="2018-12-31")

micro_2019 <- sub_micro_HTD %>%  mutate(SPEC_DATE = format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y-%m-%d")) %>%  
  filter(SPEC_DATE > "2018-12-31" & SPEC_DATE <="2019-12-31")

micro_2020 <- sub_micro_HTD %>%  mutate(SPEC_DATE = format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y-%m-%d")) %>%  
  filter(SPEC_DATE > "2019-12-31" & SPEC_DATE <="2020-12-31")

micro_2021 <- sub_micro_HTD %>%  mutate(SPEC_DATE = format(as.Date(sub_micro_HTD$SPEC_DATE), "%Y-%m-%d")) %>%  
  filter(SPEC_DATE > "2020-12-31" & SPEC_DATE <="2021-12-31")  #update 2021-05


#Merging by sex, birth
sub_2017 <- full_join(admin_2017, micro_2017, by=c("year.birth"="DATE_BIRTH", "gender"="SEX")) 
sub_2017_merge <- sub_2017 %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  distinct(id.mr,id.insurance_card, LAST_NAME, year.birth, gender, ORGANISM_CODE, SPECIMEN_CODE, .keep_all = TRUE)


sub_2018 <- full_join(admin_2018, micro_2018, by=c("year.birth"="DATE_BIRTH", "gender"="SEX")) 
sub_2018_merge <- sub_2018 %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  distinct(id.mr,id.insurance_card, LAST_NAME, year.birth, gender, ORGANISM_CODE, SPECIMEN_CODE, .keep_all = TRUE)


sub_2019 <- full_join(admin_2019, micro_2019, by=c("year.birth"="DATE_BIRTH", "gender"="SEX")) 
sub_2019_merge <- sub_2019 %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  distinct(id.mr,id.insurance_card, LAST_NAME, year.birth, gender, ORGANISM_CODE, SPECIMEN_CODE, .keep_all = TRUE)


sub_2020 <- full_join(admin_2020, micro_2020, by=c("year.birth"="DATE_BIRTH", "gender"="SEX")) 
sub_2020_merge <- sub_2020 %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  distinct(id.mr,id.insurance_card, LAST_NAME, year.birth, gender, ORGANISM_CODE, SPECIMEN_CODE, .keep_all = TRUE)


sub_2021 <- full_join(admin_2021, micro_2021, by=c("year.birth"="DATE_BIRTH", "gender"="SEX")) 
sub_2021_merge <- sub_2021 %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  distinct(id.mr,id.insurance_card, LAST_NAME, year.birth, gender, ORGANISM_CODE, SPECIMEN_CODE, .keep_all = TRUE)


HTD_merge_name <- rbind(sub_2017_merge, sub_2018_merge, sub_2019_merge, sub_2020_merge, sub_2021_merge) %>% 
  mutate(SPEC_DATE=format(as.Date(SPEC_DATE), "%Y-%m-%d"))

janitor::compare_df_cols(same_ID_record, HTD_merge_name)
HTD_merge_name$PATIENT_ID<-NULL
same_ID_record <- same_ID_record %>%  subset(-c("SEX", "DATE_BIRTH"))

Linkdata2<-rbind(same_ID_record, HTD_merge_name)
saveRDS(Linkdata, "Linkdata_HTD.Rds")
saveRDS(same_ID_record, "HTD_merge_byID_Ha_0407.Rds")


####------------NHTD dataset---------#######
#deduplication micro Data
micro_NHTD <- readRDS("NHTD_interpret_all_final_COMPLETE.Rds") %>% 
  #delete ending "KC,GP"
  mutate(PATIENT_ID=substr(PATIENT_ID,1,9))

#checking NA in patient id, organism, and specimen
sum(is.na(micro_NHTD$PATIENT_ID))  #0
sum(is.na(micro_NHTD$SPECIMEN))  #0
sum(is.na(micro_NHTD$ORGANISM))  #0

micro_NHTD$group <- paste(micro_NHTD$PATIENT_ID,micro_NHTD$ORGANISM, micro_NHTD$SPECIMEN, sep = "-")
micro_NHTD$SPEC_DATE <- as.Date(micro_NHTD$SPEC_DATE,"%Y-%m-%d")

dedup_micro_nhtd <- micro_NHTD[0,]

patientOnlyOneIsolate <- micro_NHTD %>% filter(group %in% (table(micro_NHTD$group)[table(micro_NHTD$group)==1] %>% names()))
patientMoreThanOneIsolate <- micro_NHTD %>% filter(group %in% (table(micro_NHTD$group)[table(micro_NHTD$group)>1] %>% names()))

for (isolatesgroup in unique(patientMoreThanOneIsolate$group)){
  isolatesdf <- subset(x = micro_NHTD,group==isolatesgroup)
  dedup_micro_nhtd <- rbind(dedup_micro_nhtd,dedup(isolatesdf))
}

micro_NHTD_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_nhtd) #0NA patient id
saveRDS(micro_NHTD_dedup_all, "Micro_NHTD_dedup_final.Rds")

a <- as.data.frame(table(micro_NHTD$PATIENT_ID))
a$stt<-1:nrow(a)
b <- as.data.frame(table(micro_NHTD_dedup_all$PATIENT_ID))
sum(b$Freq >=5, na.rm = TRUE)

##Loading admin data (0NA date.admid, year. birth, id.mr)
admin_NHTD<-readRDS("NHTD_2020_admin_with_name2023-05-26.rds") %>% 
  mutate(date.admission=format(as.Date(date.admission), "%Y-%m-%d")) %>% 
  mutate(id.mr=substr(id.mr, 3,11)) 

#Merging by ID
merged_ID_NHTD <- full_join(admin_NHTD, Micro_NHTD_dedup_all, by = c("id.mr" = "PATIENT_ID"))

same_ID_record <- merged_ID_NHTD %>% 
  distinct(id.mr, name, year.birth, 
           date.admission, SPEC_DATE, WARD, SPECIMEN, ORGANISM, .keep_all = TRUE) %>%
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=60) %>% 
  saveRDS(., "Linkdata_NHTD.Rds")


Linkdata_NHTD_Ha_270623<- Linkdata_NHTD_Ha_270623 %>% mutate(enter_hospital = paste0(id.patient, date.admission))

a <- Linkdata_NHTD_Ha_270623 <- Linkdata_NHTD_Ha_270623 %>%
  group_by(id.patient, enter_hospital) %>% 
  filter(Enterobacterales == "Enterobacterale") 

a <- Linkdata_NHTD_Ha_270623 %>% 
  group_by(id.patient) %>% 
  filter_at(vars(ESBL_E_or), any_vars( . == "Resistant"))



####------------VT dataset---------#######
#Micro data
micro_VT <- readRDS("VT_interpret_all_final_COMPLETE.Rds") %>% 
  mutate(PATIENT_ID = ifelse(grepl("[A-Za-z]|,", PATIENT_ID), "", PATIENT_ID))

micro_VT$group <- paste(micro_VT$PATIENT_ID,micro_VT$ORGANISM, micro_VT$SPECIMEN, sep = "-")
micro_VT$SPEC_DATE <- as.Date(micro_VT$SPEC_DATE,"%Y-%m-%d")

dedup_micro_VT <- micro_VT[0,]

patientOnlyOneIsolate <- micro_VT %>% filter(group %in% (table(micro_VT$group)[table(micro_VT$group)==1] %>% names()))
patientMoreThanOneIsolate <- micro_VT %>% filter(group %in% (table(micro_VT$group)[table(micro_VT$group)>1] %>% names()))

for (isolatesgroup in unique(patientMoreThanOneIsolate$group)){
  isolatesdf <- subset(x = micro_VT,group==isolatesgroup)
  dedup_micro_VT <- rbind(dedup_micro_VT,dedup(isolatesdf))
}

micro_VT_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_VT)
saveRDS(micro_VT_dedup_all, "Micro_VT_dedup_final.Rds")


#age 220NA, SPEC_DATE 31NA, 
micro_VT_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_VT) %>% 
  mutate(name_initial=str_to_title(paste(LAST_NAME, FIRST_NAME, sep=" "))) %>%
  mutate(name_initial=gsub("[^A-Z]+", "", name_initial)) %>% 
  mutate(STT=seq.int(nrow(.))) %>% 
  mutate(SEX=case_when(SEX=="F"~"f",
                       SEX=="M"~"m")) %>% 
  mutate(year = as.numeric(format(as.Date(SPEC_DATE), "%Y"))) %>% 
  mutate(AGE = year-AGE) %>% 
  filter(SPEC_DATE >= "2017-01-01")
  

##Loading admin data (gender 97NA, year.birth 0NA, date.admission 0NA)
admin_VT<-readRDS("VTH_2017_2020_admin_with_name2023-05-26.rds") %>% 
  mutate(date.admission=format(as.Date(date.admission), "%Y-%m-%d")) %>% 
  mutate(name_initial_admin=str_to_title(stri_trans_general(name, id = "Latin-ASCII"))) %>% 
  mutate(name_initial_admin=gsub("[^A-Z]+", "", name_initial_admin)) %>% 
  mutate(gender=case_when(gender=="Female"~"f",
                       gender=="Male"~"m")) %>% 
  mutate(year.birth = as.numeric(year.birth)) %>% 
  filter(date.admission >= "2016-12-22")
  

#Merging data by ID (id.mr/ id.patient) - cannot merged by ID
merged_ID_VT <- full_join(admin_VT, micro_VT_dedup_all, by = c("id.patient" = "PATIENT_ID"))

t<- merged_ID_VT %>%  select(id.patient, date.admission, SPEC_DATE)

#data merged successful with same patient ID
same_ID_record <- merged_ID_VT %>% #filter(!is.na(STT) & !is.na(id.patient)) %>% 
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  distinct(id.patient, name_initial_admin, date.admission, SPEC_DATE, ORGANISM, SPECIMEN, .keep_all = TRUE) %>% 
  filter(date_differ>=0&date_differ<=7) 
list_sameID <- unique(same_ID_record$id.patient)


#Merging by sex, birth, name initial
VT_all<-full_join(admin_VT, micro_VT_dedup_all, by=c("year.birth"="AGE", "name_initial_admin"="name_initial", "gender"="SEX"))

tesst<-VT_all %>%
  mutate(date_differ=as.numeric((as.Date(SPEC_DATE)))- as.numeric(as.Date(date.admission))) %>% 
  filter(date_differ>=0&date_differ<=7) %>% 
  distinct(id.patient, name_initial_admin, year.birth, gender, WARD, SPECIMEN, ORGANISM, .keep_all = TRUE)

saveRDS(VT_all, "Linkdata_VT.Rds")

  






