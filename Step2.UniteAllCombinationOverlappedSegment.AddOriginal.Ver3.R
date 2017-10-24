setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

cross_num = 5
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
file_prefix_original = "ctdd-ctdc-ctdt-aac-divider-1-"
file_prefix_overlapped = "ALL-overlapped-"

for(file_i in file_name_pattern){
  for(cross_i in 1:cross_num){
    if(exists("main_data.basket")){
      rm("main_data.basket")
    }
    
    file_original = paste0("combination/", file_prefix_original,file_i,cross_i,".txt")
    main_data.original = read.csv(file_original, stringsAsFactors = FALSE)
    
    file_overlapped = paste0("overlapped/result/", file_prefix_overlapped,file_i,cross_i,".csv")
    main_data.overlapped = read.csv(file_overlapped, stringsAsFactors = FALSE)
    
    if(!identical(as.character(main_data.original$class), as.character(main_data.overlapped$class))){
      print(paste(file_original, file_overlapped, "FALSE ==============="))
    } else {
      main_data.basket = cbind.data.frame(main_data.original[,-ncol(main_data.original)], main_data.overlapped[,-ncol(main_data.overlapped)], main_data.original$class)
      colnames(main_data.basket)[ncol(main_data.basket)] = "class"
      write.csv(main_data.basket, paste0("overlapped/all/ALL-original-overlapped-",file_i,cross_i,".csv"), row.names = FALSE)
    }
  }
}