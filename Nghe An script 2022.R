require(dplyr)
require(readxl)
require(AMR)
require(stringr)
library(arsenal)
rm(list=ls()) 

## -----------Uong Bi Hospital---------
NA_2022<-read_excel("C:/Users/halt.OUCRU/PH Epidemiology Dropbox/Le Thanh Ha/46HN vi sinh/46HN-vi-sinh/Nghe An/Vi sinh 2022_10042023.xlsx", guess_max = 99999)
NA_2022<-NA_2022 %>% rename(specimen=13, organism=16) %>% mutate(specimen=recode(specimen,"ur"="urin"))
table(NA_2022$specimen)


NA_2022<-cbind(NA_2022[, c(1:22)], NA_2022 %>% select(contains("_ND")|contains("_NM")|contains("_NE")))
unique(unlist(NA_2022[,c(23:82)]))  #having RSI values, having NS value
apply(NA_2022, 2, function(x) which(x == "NS")) #CTX_ND30 row 3673, 3786
str(NA_2022$CTX_ND30)
NA_2022$CTX_ND30[NA_2022$CTX_ND30=="NS"]<-"S"


NA_2022_num <- apply(NA_2022[,c(23:82)],MARGIN = 2, function(x){stringr::str_replace_all(x,"R|I|S|r|i|s","") }) %>% as.data.frame()
NA_2022_RSI <- apply(NA_2022[,c(23:82)],MARGIN = 2, function(x){stringr::str_replace_all(x,"[^RISris]","") }) %>% as.data.frame()
unique(unlist(NA_2022_RSI))

NA_2022_num<-as.data.frame(sapply(NA_2022_num, as.numeric))
NA_2022_num<-data.frame(sapply(NA_2022_num, function(x) as.character(gsub("<=","<",x))))
NA_2022_num<-data.frame(sapply(NA_2022_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(NA_2022_num))

NA_2022_num[, 1:51] <- lapply(NA_2022_num[, 1:51], as.disk)  #truncated 2 values: 63,62 "
NA_2022_num[,52:60]<- lapply(NA_2022_num[,52:60], as.mic)   #truncated 1 value: 2.5"


NA_2022_combine<-cbind(NA_2022[,c(13,16)], NA_2022_num)
unique(unlist(NA_2022_combine[,c(3:62)]))
NA_2022_combine[is.na(NA_2022_combine)]<-""   #truncated 50 "" (blank) values

NA_2022_combine<-as.rsi(NA_2022_combine, guideline = "CLSI", uti=NULL,
                        conserve_capped_values = TRUE,    
                        reference_data = AMR::rsi_translation)


NA_2022_interpret <- NA_2022_combine 
for(i in 3:ncol(NA_2022_combine)){
  NA_2022_combine[is.na(NA_2022_combine[,i]),i] <- ""
  NA_2022_RSI[is.na(NA_2022_RSI[,i-2]),i-2] <- ""
  NA_2022_interpret[,i] <- paste0(NA_2022_combine[,i],NA_2022_RSI[,i-2])
}


unique(unlist(NA_2022_interpret[,3:62]))   #NA, NAS, NAR, NAI
NA_2022_interpret[NA_2022_interpret=="NAI"]<-"I"
NA_2022_interpret[NA_2022_interpret=="NAS"]<-"S"
NA_2022_interpret[NA_2022_interpret=="NAR"]<-"R"
NA_2022_interpret[NA_2022_interpret=="NA"]<-""


#Comparing two datasets
NA_2022_interpret_final<-cbind(NA_2022[,c(1:22)], NA_2022_interpret[3:62])
summary(comparedf(x=NA_2022[23:24], y=NA_2022_interpret_final[23:24]))
summary(table(NA_2021[23:25]))
all_equal(NA_2021[c(23:79)], NA_2021_interpret_final[c(23:79)])


#checking
table(NA_2022_interpret_final$AMP_ND10)
sum(table(NA_2022$AMP_ND10)) #lost 2 values

table(NA_2022_interpret_final$VAN_NM)
table(NA_2022$VAN_NM) #same

table(NA_2022_interpret_final$MIF_ND)
table(NA_2022$MIF_ND) #same

table(NA_2022_interpret_final$TCC_ND75)
table(NA_2022$TCC_ND75) #same


saveRDS(NA_2022_interpret_final, "NgheAn_Interpret_2022_final.Rds")

