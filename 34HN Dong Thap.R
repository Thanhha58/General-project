require(dplyr)
require(readxl)
require(AMR)
require(stringr)
require(tibble)
require(plyr)
require(tidyr)
require(reshape2)
library(arsenal)
require(purrr)
options(encoding = 'UTF-8')

########## Dong Thap##############
###--------Interpreting data from 2017-2021-------
#2017
data2017<-read_excel("~/Vi sinh/34HN Dong Thap/34HN_SS1_BVDK Dong Thap_Vi sinh_2017_31032021.xlsx", guess_max = 99999)  %>% 
  mutate(SPEC_TYPE= recode(SPEC_TYPE,"ur"="urin")) 
data2017[, 20:56] <- lapply(data2017[, 20:56], as.disk) #delete 3 values 66,69,S

data2017_combine<-cbind(data2017[,c(11,13)], data2017[,c(20:56)])
data2017_combine[is.na(data2017_combine)]<-"" #delete 34 ""
data2017_combine<-as.rsi(data2017_combine, guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)

data_interpret_2017<-cbind(data2017[,c(1:19)], data2017_combine[,c(3:39)])

data_interpret_2017<-data_interpret_2017 %>% add_column(STT=NA, COUNTRY=NA, ORIGIN=NA, DATE_BIRTH=NA,PAT_TYPE=NA, INSTITUTE=NA,
                                                        DEPARTMENT=NA, WARD_TYPE=NA, SPEC_CODE=NA, SPEC_REAS=NA,INDUC_CLI=NA, 
                                                        COMMENT=NA, DATE_DATA=NA)

data_interpret_2017<-data_interpret_2017 %>% select(STT, COUNTRY, LABORATORY, ORIGIN, PATIENT_ID, LAST_NAME, FIRST_NAME, 
                                                    SEX, DATE_BIRTH, AGE, PAT_TYPE, WARD, INSTITUTE, DEPARTMENT, WARD_TYPE,
                                                    DATE_ADMIS, SPEC_NUM, SPEC_DATE, SPEC_TYPE, SPEC_CODE, SPEC_REAS, 
                                                    ISOL_NUM, ORGANISM, ORG_TYPE, SEROTYPE, BETA_LACT, ESBL, CARBAPENEM, 
                                                    MRSA_SCRN, INDUC_CLI, COMMENT, DATE_DATA, AMP_ND10:TCC_ND75)

names(data_interpret_2017)[33:69]<-paste(colnames(data_interpret_2017)[33:69] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
saveRDS(data_interpret_2017, "data_interpret_2017.Rds")




#2018
data2018<-read_excel("~/Vi sinh/34HN Dong Thap/34HN_SS1_BVDK Dong Thap_Vi sinh_2018_01042021.xlsx", guess_max = 99999) %>% 
  mutate(SPEC_TYPE=recode(SPEC_TYPE,"ur"="urin"))

data2018[,c(20:78)][is.na(data2018[,c(20:78)])]<-"" 
data2018[, c(20:78)] <- lapply(data2018[, c(20:78)], as.numeric)

data2018[, c(20:53,66:78)] <- lapply(data2018[,c(20:53,66:78)], as.disk) #delele 2 values 69,66
data2018[, 54:65] <- lapply(data2018[, 54:65], as.mic)
data2018_combine<-cbind(data2018[,c(11,13)], data2018[,c(20:78)])

data2018_combine[is.na(data2018_combine)]<-""  #delete 47 ""
data2018_combine<-as.rsi(data2018_combine, guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)

data_interpret_2018<-cbind(data2018[,c(1:19)], data2018_combine[,c(3:61)])


data_interpret_2018<-data_interpret_2018 %>% add_column(STT=NA, COUNTRY=NA, ORIGIN=NA, DATE_BIRTH=NA,PAT_TYPE=NA, INSTITUTE=NA,
                                                        DEPARTMENT=NA, WARD_TYPE=NA, SPEC_CODE=NA, SPEC_REAS=NA,INDUC_CLI=NA, 
                                                        COMMENT=NA, DATE_DATA=NA)

data_interpret_2018<-data_interpret_2018 %>% select(STT, COUNTRY, LABORATORY, ORIGIN, PATIENT_ID, LAST_NAME, FIRST_NAME, 
                                                    SEX, DATE_BIRTH, AGE, PAT_TYPE, WARD, INSTITUTE, DEPARTMENT, WARD_TYPE,
                                                    DATE_ADMIS, SPEC_NUM, SPEC_DATE, SPEC_TYPE, SPEC_CODE, SPEC_REAS, 
                                                    ISOL_NUM, ORGANISM, ORG_TYPE, SEROTYPE, BETA_LACT, ESBL, CARBAPENEM, 
                                                    MRSA_SCRN, INDUC_CLI, COMMENT, DATE_DATA, AMP_ND10:TCC_ND75)

name2018_disk1<-paste(colnames(data_interpret_2018)[33:66] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
name2018_disk2<-paste(colnames(data_interpret_2018)[79:91] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2018)[33:66]=name2018_disk1
names(data_interpret_2018)[79:91]=name2018_disk2
name2018_mic<-paste(colnames(data_interpret_2018)[67:78] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))
names(data_interpret_2018)[67:78]=name2018_mic

saveRDS(data_interpret_2018, "data_interpret_2018.Rds")




#2019
data2019<-read_excel("~/Vi sinh/34HN Dong Thap/34HN_SS1_BVDK Dong Thap_Vi sinh_2019_01042021.xlsx", guess_max = 99999) %>% 
  mutate(SPEC_TYPE=recode(SPEC_TYPE,"ur"="urin"))
table(data2019$specimen)
data2019[, 20:54] <- lapply(data2019[, 20:54], as.disk)  #delete 2 "I"

data2019_combine<-cbind(data2019[,c(11,13)], data2019[,c(20:54)])
data2019_combine[is.na(data2019_combine)]<-"" 
data2019_combine<-as.rsi(data2019_combine, guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)

data_interpret_2019<-cbind(data2019[,c(1:19)], data2019_combine[,c(3:37)])

data_interpret_2019<-data_interpret_2019 %>% add_column(STT=NA, COUNTRY=NA, ORIGIN=NA, DATE_BIRTH=NA,PAT_TYPE=NA, INSTITUTE=NA,
                                                        DEPARTMENT=NA, WARD_TYPE=NA, SPEC_CODE=NA, SPEC_REAS=NA,INDUC_CLI=NA, 
                                                        COMMENT=NA, DATE_DATA=NA)

data_interpret_2019<-data_interpret_2019 %>% select(STT, COUNTRY, LABORATORY, ORIGIN, PATIENT_ID, LAST_NAME, FIRST_NAME, 
                                                    SEX, DATE_BIRTH, AGE, PAT_TYPE, WARD, INSTITUTE, DEPARTMENT, WARD_TYPE,
                                                    DATE_ADMIS, SPEC_NUM, SPEC_DATE, SPEC_TYPE, SPEC_CODE, SPEC_REAS, 
                                                    ISOL_NUM, ORGANISM, ORG_TYPE, SEROTYPE, BETA_LACT, ESBL, CARBAPENEM, 
                                                    MRSA_SCRN, INDUC_CLI, COMMENT, DATE_DATA, AMP_ND10:COL_ND10)

name2019<-paste(colnames(data_interpret_2019)[33:66] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2019)[33:66]=name2019
saveRDS(data_interpret_2019, "data_interpret_2019.Rds")



#2020
data2020<-read_excel("~/Vi sinh/34HN Dong Thap/34HN_SS1_BVDK Dong Thap_Vi sinh_2020_02042021.xlsx", guess_max = 99999) %>% 
  mutate(SPEC_TYPE=recode(SPEC_TYPE,"ur"="urin"))

data2020[,c(20:56)][is.na(data2020[,c(20:56)])]<-"" 
data2020[, c(20:56)] <- lapply(data2020[, c(20:56)], as.numeric)
data2020[, 20:52] <- lapply(data2020[, 20:52], as.disk)
data2020[, 53:56] <- lapply(data2020[, 53:56], as.mic)


data2020_combine<-cbind(data2020[,c(11,13)], data2020[,c(20:56)])
data2020_combine[is.na(data2020_combine)]<-"" #delete 33 ""
data2020_combine<-as.rsi(data2020_combine, guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)

data_interpret_2020<-cbind(data2020[,c(1:19)], data2020_combine[,c(3:39)])


data_interpret_2020<-data_interpret_2020 %>% add_column(STT=NA, COUNTRY=NA, ORIGIN=NA, DATE_BIRTH=NA,PAT_TYPE=NA, INSTITUTE=NA,
                                                        DEPARTMENT=NA, WARD_TYPE=NA, SPEC_CODE=NA, SPEC_REAS=NA,INDUC_CLI=NA, 
                                                        COMMENT=NA, DATE_DATA=NA)

data_interpret_2020<-data_interpret_2020 %>% select(STT, COUNTRY, LABORATORY, ORIGIN, PATIENT_ID, LAST_NAME, FIRST_NAME, 
                                                    SEX, DATE_BIRTH, AGE, PAT_TYPE, WARD, INSTITUTE, DEPARTMENT, WARD_TYPE,
                                                    DATE_ADMIS, SPEC_NUM, SPEC_DATE, SPEC_TYPE, SPEC_CODE, SPEC_REAS, 
                                                    ISOL_NUM, ORGANISM, ORG_TYPE, SEROTYPE, BETA_LACT, ESBL, CARBAPENEM, 
                                                    MRSA_SCRN, INDUC_CLI, COMMENT, DATE_DATA, AMP_ND10:MEM_NM)

name2020_disk<-paste(colnames(data_interpret_2020)[33:65] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2020)[33:65]=name2020_disk
name2020_mic<-paste(colnames(data_interpret_2020)[66:69] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))
names(data_interpret_2020)[66:69]=name2020_mic
saveRDS(data_interpret_2020, "data_interpret_2020.Rds")



#2021
data2021<-read_excel("~/Vi sinh/34HN Dong Thap/34HN_SS1_BVDK Dong Thap_Vi sinh_2021_01072022.xlsx", guess_max = 99999) %>%
  mutate(SPEC_TYPE=recode(SPEC_TYPE,"ur"="urin"))

data2021[,c(32:123)][is.na(data2021[,c(32:123)])]<-"" 
data2021[, c(32:123)] <- lapply(data2021[, c(32:123)], as.numeric)
data2021[, c(32:61,63,74:76,78:90)] <- lapply(data2021[, c(32:61,63,74:76,78:90)], as.disk) #delete 5 outliners
data2021[, c(62,64:73,77,91:123)] <- lapply(data2021[, c(62,64:73,77,91:123)], as.mic) #delete 2 outliners

data2021_combine<-cbind(data2021[,c(18,22)], data2021[,c(32:123)])
data2021_combine[is.na(data2021_combine)]<-""   #delete 47 outliners
data2021_combine<-as.rsi(data2021_combine, guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)

data_interpret_2021<-cbind(data2021[,c(1:31)], data2021_combine[,c(3:94)])
data_interpret_2021<-data_interpret_2021 %>% add_column(STT=NA)
names(data_interpret_2021)[1]<-"COUNTRY"
names(data_interpret_2021)[12]<-"INSTITUTE"

data_interpret_2021<-data_interpret_2021 %>% select(STT, COUNTRY, LABORATORY, ORIGIN, PATIENT_ID, LAST_NAME, FIRST_NAME, 
                                                    SEX, DATE_BIRTH, AGE, PAT_TYPE, WARD, INSTITUTE, DEPARTMENT, WARD_TYPE,
                                                    DATE_ADMIS, SPEC_NUM, SPEC_DATE, SPEC_TYPE, SPEC_CODE, SPEC_REAS, 
                                                    ISOL_NUM, ORGANISM, ORG_TYPE, SEROTYPE, BETA_LACT, ESBL, CARBAPENEM, 
                                                    MRSA_SCRN, INDUC_CLI, COMMENT, DATE_DATA, AMP_ND10:SXT_NM)

name2021_disk1<-paste(colnames(data_interpret_2021)[33:62] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2021)[33:62]=name2021_disk1
names(data_interpret_2021)[64]<-"LNZ_ND"
name2021_disk2<-paste(colnames(data_interpret_2021)[75:77] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2021)[75:77]=name2021_disk2
name2021_disk3<-paste(colnames(data_interpret_2021)[79:91] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(data_interpret_2021)[79:91]=name2021_disk3

saveRDS(data_interpret_2021, "data_interpret_2021.Rds")

all_data<-gtools::smartbind(data_interpret_2017, data_interpret_2018, data_interpret_2019, data_interpret_2020, data_interpret_2021)
unique(unlist(all_data[,33:123]))
saveRDS(all_data, "DTH_interpret_1721.Rds")


## Cleaning data 2014, 2015, 2016
data2014<-read_xlsx("34HN-SS1_Dong Thap_2014_clean.xlsx") 
data2015<-read_xlsx("34HN-SS1_Dong Thap_2015_clean.xlsx")
data2016<-read_xlsx("34HN-SS1_Dong Thap_2016_clean.xlsx")

data<-rbind(data2014, data2015, data2016)
a<-as.data.frame(unique(data[9]))
b<-as.data.frame(unique(data[10]))

a$specimen<-c("ps","sp","ta","urin","bl","sf",
              "an","st","ba","va", "bi","th", "ab","bi","lo","dn","wa", "tr","dn",
              "ga","pf","fl","ca", "am","ki","cx")

b$organism<-c("pae","eco", "ac-","ent","sau","kl-","sp-","pmi", "pvu","en-",
              "scn", "oth","spn", "pce", "shd","sgc","se-","ci-","sui",
              "shc","ent","svi","spy","pvu","bs-","shb","eta","sat","sap","dxe","sal","pvu","cvi")


data<-left_join(data, a, by="Bá»NH PHáº¨M")
data<-left_join(data, b, by="Vi Khuáº©n")
data<- data %>% relocate(specimen, .before=ESBL)
data<- data %>% relocate(organism, .before=ESBL)
data[,9]<-NULL
data[,9]<-NULL
names(data)

unique(unlist(data[,13:51]))  # E, SS, O, T
data[,13:51][data[,13:51]=="E"]<-""
data[,13:51][data[,13:51]=="T"]<-""
data[,13:51][data[,13:51]=="O"]<-""
data[,13:51][data[,13:51]=="NA"]<-""
data[,13:51][data[,13:51]=="SS"]<-"S"

names(data)[1:8]<-c("stt", "ho", "ten", "age","date_and_birth","sex","khoa","ngay_cay_mau")

saveRDS(data, "DTH_interpret_1416.Rds")
write.csv(as.data.frame(colnames(data[,1:12])), "Name_DongThap_1416.csv")





