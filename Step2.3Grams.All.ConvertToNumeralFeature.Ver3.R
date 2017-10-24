setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)

main_data.files = cbind(c("chlo1.txt","chlo2.txt","chlo3.txt","chlo4.txt","chlo5.txt"))
main_data.files = rbind(main_data.files, cbind(c("cytop1.txt","cytop2.txt","cytop3.txt","cytop4.txt","cytop5.txt")))
main_data.files = rbind(main_data.files, cbind(c("cytos1.txt","cytos2.txt","cytos3.txt","cytos4.txt","cytos5.txt")))
main_data.files = rbind(main_data.files, cbind(c("er1.txt","er2.txt","er3.txt","er4.txt","er5.txt")))
main_data.files = rbind(main_data.files, cbind(c("extr1.txt","extr2.txt","extr3.txt","extr4.txt","extr5.txt")))
main_data.files = rbind(main_data.files, cbind(c("golgi1.txt","golgi2.txt","golgi3.txt","golgi4.txt","golgi5.txt")))
main_data.files = rbind(main_data.files, cbind(c("lyso1.txt","lyso2.txt","lyso3.txt","lyso4.txt","lyso5.txt")))
main_data.files = rbind(main_data.files, cbind(c("mito1.txt","mito2.txt","mito3.txt","mito4.txt","mito5.txt")))
main_data.files = rbind(main_data.files, cbind(c("nuc1.txt","nuc2.txt","nuc3.txt","nuc4.txt","nuc5.txt")))
main_data.files = rbind(main_data.files, cbind(c("pero1.txt","pero2.txt","pero3.txt","pero4.txt","pero5.txt")))
main_data.files = rbind(main_data.files, cbind(c("plas1.txt","plas2.txt","plas3.txt","plas4.txt","plas5.txt")))
main_data.files = rbind(main_data.files, cbind(c("vacu1.txt","vacu2.txt","vacu3.txt","vacu4.txt","vacu5.txt")))

provec_data = read.table("3grams/protVec_100d_3grams.csv", header = FALSE, stringsAsFactors = FALSE)

for(file_i in main_data.files){
  if(exists("main_data.file.numerical")){
    rm("main_data.file.numerical")
  }
  
  main_data = read.csv(paste0("3grams/",file_i), stringsAsFactors = FALSE)
  
  for(i in 1:nrow(main_data)){
    if(exists("main_data.numerical")){
      rm("main_data.numerical")
    }
    
    datum = main_data[i,]
    seq = paste(datum$seq1, datum$seq2, datum$seq3)
    print(seq)
    
    datum.text = data_frame(txt = seq)
    datum.token = unnest_tokens(datum.text, word, txt, to_lower = FALSE)
    
    for(j in 1:nrow(datum.token)){
      seq3gram = as.character(datum.token[j,1])
      seq3gram_feature = provec_data[which(provec_data$V1 == seq3gram),]
      
      if(nrow(seq3gram_feature) > 0){
        if(!exists("main_data.numerical")){
          assign("main_data.numerical", seq3gram_feature)
        } else {
          main_data.numerical = rbind.data.frame(main_data.numerical, seq3gram_feature)
        }
      }
    }
    
    main_data.numerical_sum = colSums(main_data.numerical[,-1])
    main_data.numerical_temp = cbind(rbind(main_data.numerical_sum), datum$class)
    
    if(!exists("main_data.file.numerical")){
      assign("main_data.file.numerical", main_data.numerical_temp)
    } else {
      main_data.file.numerical = rbind.data.frame(main_data.file.numerical, main_data.numerical_temp)
    }
  }
  
  colnames(main_data.file.numerical) = c(paste0("V", c(1:ncol(main_data.file.numerical))))
  colnames(main_data.file.numerical)[ncol(main_data.file.numerical)] = "class"
  
  write.csv(main_data.file.numerical, paste0("3grams/numerical_all/numerical_all.", file_i), row.names = FALSE, quote = FALSE)
}