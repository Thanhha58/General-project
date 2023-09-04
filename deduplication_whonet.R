library(dplyr)

'%ni%' <- Negate('%in%')


readRDS(file = "C:\\Users\\dungvtv\\Documents\\Zalo Received Files\\NHTD_interpret_all_final_COMPLETE.Rds") -> micro_NHTD

micro_NHTD <- NHTD_interpret_all_final_COMPLETE %>% 
  mutate(PATIENT_ID = substr(PATIENT_ID,1,9))

   micro_NHTD$group <- paste(micro_NHTD$PATIENT_ID,micro_NHTD$ORGANISM, micro_NHTD$SPECIMEN, sep = "-")
   micro_NHTD$SPEC_DATE <- as.Date(micro_NHTD$SPEC_DATE,"%Y-%m-%d")
   
   dedup_micro_nhtd <- micro_NHTD[0,]
   
   patientOnlyOneIsolate <- micro_NHTD %>% filter(group %in% (table(micro_NHTD$group)[table(micro_NHTD$group)==1] %>% names()))
   patientMoreThanOneIsolate <- micro_NHTD %>% filter(group %in% (table(micro_NHTD$group)[table(micro_NHTD$group)>1] %>% names()))
   
   for (isolatesgroup in unique(patientMoreThanOneIsolate$group)){
     isolatesdf <- subset(x = micro_NHTD,group==isolatesgroup)
     dedup_micro_nhtd <- rbind(dedup_micro_nhtd,dedup(isolatesdf))
   }
   
   micro_NHTD_dedup_all <- rbind(patientOnlyOneIsolate,dedup_micro_nhtd)
   
   ######
   
  
   dedup <-  function(x,window=30){
    x <- x[order(x$SPEC_DATE),]
    x$ord <- 1:nrow(x)
    # 
    ord <- tidyr::crossing(x$ord,x$ord) %>% as.data.frame
    names(ord) <- c("order1","order2")
    tmp <- tidyr::expand_grid(x[,"SPEC_DATE"],x[,"SPEC_DATE"]) %>% as.data.frame
    names(tmp) <- c("specdate1","specdate2")
    tmp <- cbind(tmp,ord)
    tmp <- tmp %>% filter(order2 > order1)
    #tmp$datediffer <- as.Date(tmp$specdate2,"%Y-%m-%d") - as.Date(tmp$specdate1,"%Y-%m-%d")
    tmp$datediffer <- tmp$specdate2 - tmp$specdate1
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



