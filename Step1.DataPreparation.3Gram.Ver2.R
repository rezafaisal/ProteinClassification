setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(protr)
library(stringr)

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



for(file_i in main_data.files){
  main_data = read.csv(paste0("dataset/",file_i), stringsAsFactors = FALSE)
  
  if(exists("main_data.file")) {
    rm("main_data.file")
  }
  
  for(i in 1:nrow(main_data)){
    seq = ""
    
    if(exists("main_data.seq")) {
      rm("main_data.seq")
    }
    
    for(seq_i in 1:3){
      if(seq_i == 1){
        seq = main_data[i,]$seq
      } else {
        seq = paste0(substr(seq, 2, nchar(seq)), substr(seq, 1, 1))
      }
      
      if(!exists("main_data.seq")){
        assign("main_data.seq", gsub("(.{3})", "\\1 ", seq))
      } else {
        main_data.seq = cbind(main_data.seq, gsub("(.{3})", "\\1 ", seq))
      }
      
      print(seq)
      print(paste(nchar(seq), gsub("(.{3})", "\\1 ", seq)))
    }
    
    main_data.file_temp = cbind(main_data.seq, main_data[i,]$class)
    if(!exists("main_data.file")){
      assign("main_data.file", main_data.file_temp)
    } else {
      main_data.file = rbind.data.frame(main_data.file, main_data.file_temp)
    }
    
    print("------------------------------------------------------------")
    
  }
  colnames(main_data.file) = c("seq1", "seq2", "seq3", "class")
  write.csv(main_data.file, paste0("3grams/", file_i), row.names = FALSE, quote = FALSE)
}


