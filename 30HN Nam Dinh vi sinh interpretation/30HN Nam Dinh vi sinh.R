library(readxl)
library(stringi)
library(stringr)
library(dplyr)
library(AMR)
require(devtools)

# Reinstall AMR older version
packageurl <- "https://cran.r-project.org/src/contrib/Archive/AMR/AMR_1.8.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")


# Importing datasets
thang1_6 <- read_excel("Nam dinh vi sinh/thang1_6.xlsx")
thang7 <- read_excel("Nam dinh vi sinh/thang7.xlsx")
thang8_11 <- read_excel("Nam dinh vi sinh/thang8_11.xlsx")


#cleaning column names
column_name <- function(x){
  x <- stri_trans_general(x, id="Latin-ASCII")
  x <- toupper(x)
  return(x)
}

colnames(thang1_6)[c(1:27)]<- column_name(colnames(thang1_6)[c(1:27)])
colnames(thang7)[c(1:27)]<- column_name(colnames(thang7)[c(1:27)])
colnames(thang8_11)[c(1:27)]<- column_name(colnames(thang8_11)[c(1:27)])


#specimens and organism
specimen <- function(x){
  x %>% 
    rename(SPECIMEN=13, ORGANISM=16) %>% 
    mutate(SPECIMEN=recode(SPECIMEN,"ur"="urin"))
}

thang1_6 <- specimen(thang1_6)
thang7 <- specimen(thang7)
thang8_11 <- specimen(thang8_11)


#changing disk/mic/etest type
names(thang1_6)[28:69]<-paste(colnames(thang1_6)[28:69] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(thang1_6)[70:117]<-paste(colnames(thang1_6)[70:117] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))

names(thang7)[28:69] <-paste(colnames(thang7)[28:69] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(thang7)[70:117]<-paste(colnames(thang7)[70:117] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))

names(thang8_11)[28:69]<-paste(colnames(thang8_11)[28:69] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(thang8_11)[70:118]<-paste(colnames(thang8_11)[70:118] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))


#separating dataset
interpret <- function(x){
  as.rsi(x, guideline = "CLSI", uti=NULL,
         conserve_capped_values = TRUE,    
         reference_data = AMR::rsi_translation)
  return(x)
}


#thang1-6
thang1_6_num <- apply(thang1_6[,c(28:117)],MARGIN = 2, function(x){stringr::str_replace_all(x,"R|I|S|r|i|s","") }) %>% as.data.frame()
thang1_6_RSI <- apply(thang1_6[,c(28:117)],MARGIN = 2, function(x){stringr::str_replace_all(x,"[^RISris]","") }) %>% as.data.frame()
unique(unlist(thang1_6_RSI))
thang1_6_num<-as.data.frame(sapply(thang1_6_num, as.numeric))
thang1_6_num<-data.frame(sapply(thang1_6_num, function(x) as.character(gsub("<=","<",x))))
thang1_6_num<-data.frame(sapply(thang1_6_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(thang1_6_num))

thang1_6_num[, 1:42] <- lapply(thang1_6_num[, 1:42], as.disk) #truncated 56
thang1_6_num[,43:90]<- lapply(thang1_6_num[,43:90], as.mic) 

thang1_6_combine<-cbind(thang1_6[,c(13,16)], thang1_6_num)
unique(unlist(thang1_6_combine[,c(3:92)]))
thang1_6_combine[is.na(thang1_6_combine)]<-""


thang1_6_combine <- as.rsi(thang1_6_combine, guideline = "CLSI", uti=NULL,
         conserve_capped_values = TRUE,    
         reference_data = AMR::rsi_translation)


thang1_6_interpret <- thang1_6_combine 
for(i in 3:ncol(thang1_6_combine)){
  thang1_6_combine[is.na(thang1_6_combine[,i]),i] <- ""
  thang1_6_RSI[is.na(thang1_6_RSI[,i-2]),i-2] <- ""
  thang1_6_interpret[,i] <- paste0(thang1_6_combine[,i],thang1_6_RSI[,i-2])
}

unique(unlist(thang1_6_interpret[,3:92]))   #NA, NAR
thang1_6_interpret[thang1_6_interpret=="NAR"]<-"R"
thang1_6_interpret[thang1_6_interpret=="NA"]<-""

thang1_6_final <- cbind(thang1_6[c(1:12,14,15,17:27)], thang1_6_interpret)

#thang7
thang7_num <- apply(thang7[,c(28:117)],MARGIN = 2, function(x){stringr::str_replace_all(x,"R|I|S|r|i|s","")}) %>% as.data.frame()
thang7_RSI <- apply(thang7[,c(28:117)],MARGIN = 2, function(x){stringr::str_replace_all(x,"[^RISris]","") }) %>% as.data.frame()
unique(unlist(thang7_RSI))

thang7_num<-as.data.frame(sapply(thang7_num, as.numeric))
thang7_num<-data.frame(sapply(thang7_num, function(x) as.character(gsub("<=","<",x))))
thang7_num<-data.frame(sapply(thang7_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(thang7_num))

thang7_num[, 1:42] <- lapply(thang7_num[, 1:42], as.disk) 
thang7_num[,43:90]<- lapply(thang7_num[,43:90], as.mic) 

thang7_combine<-cbind(thang7[,c(13,16)], thang7_num)
unique(unlist(thang7_combine[,c(3:92)]))

thang7_combine <- as.rsi(thang7_combine, guideline = "CLSI", uti=NULL,
                           conserve_capped_values = TRUE,    
                           reference_data = AMR::rsi_translation)


thang7_interpret <- thang7_combine 
for(i in 3:ncol(thang7_combine)){
  thang7_combine[is.na(thang7_combine[,i]),i] <- ""
  thang7_RSI[is.na(thang7_RSI[,i-2]),i-2] <- ""
  thang7_interpret[,i] <- paste0(thang7_combine[,i],thang7_RSI[,i-2])
}


unique(unlist(thang7_interpret[,3:92])) 
thang7_final <- cbind(thang7[c(1:12,14,15,17:27)], thang7_interpret)


#thang8-11
thang8_11_num <- apply(thang8_11[,c(28:118)],MARGIN = 2, function(x){stringr::str_replace_all(x,"R|I|S|r|i|s","") }) %>% as.data.frame()
thang8_11_RSI <- apply(thang8_11[,c(28:118)],MARGIN = 2, function(x){stringr::str_replace_all(x,"[^RISris]","") }) %>% as.data.frame()
unique(unlist(thang8_11_RSI))

thang8_11_num<-as.data.frame(sapply(thang8_11_num, as.numeric))
thang8_11_num<-data.frame(sapply(thang8_11_num, function(x) as.character(gsub("<=","<",x))))
thang8_11_num<-data.frame(sapply(thang8_11_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(thang8_11_num))

thang8_11_num[, 1:42] <- lapply(thang8_11_num[, 1:42], as.disk)  #truncated 66,69
thang8_11_num[,43:91]<- lapply(thang8_11_num[,43:91], as.mic)    #truncated 11,15,21,25

thang8_11_combine<-cbind(thang8_11[,c(13,16)], thang8_11_num)
unique(unlist(thang8_11_combine[,c(3:93)]))
thang8_11_combine[is.na(thang8_11_combine)]<-"" 


thang8_11_combine <- as.rsi(thang8_11_combine, guideline = "CLSI", uti=NULL,
                           conserve_capped_values = TRUE,    
                           reference_data = AMR::rsi_translation)


thang8_11_interpret <- thang8_11_combine 
for(i in 3:ncol(thang8_11_combine)){
  thang8_11_combine[is.na(thang8_11_combine[,i]),i] <- ""
  thang8_11_RSI[is.na(thang8_11_RSI[,i-2]),i-2] <- ""
  thang8_11_interpret[,i] <- paste0(thang8_11_combine[,i],thang8_11_RSI[,i-2])
}

unique(unlist(thang8_11_interpret[,3:93]))   #NA, NAR
thang8_11_interpret[thang8_11_interpret=="NAR"]<-"R"
thang8_11_interpret[thang8_11_interpret=="NA"]<-""

thang8_11_final <- cbind(thang8_11[c(1:12,14,15,17:27)], thang8_11_interpret)


#Final interpretation dataset
Namdinh_final_interpret <- bind_rows(thang1_6_final, thang7_final, thang8_11_final)
saveRDS(Namdinh_final_interpret, "Namdinh_final_interpret.Rds")


