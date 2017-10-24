setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

cross_num = 5
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")

for(file_i in file_name_pattern){
  for(cross_i in 1:cross_num){
    if(exists("class_init")){
      rm("class_init")
    }
    
    if(exists("main_data.basket")){
      rm("main_data.basket")
    }
    
    main_data.3grams = read.csv(paste0("3grams/numerical_all/numerical_all.", file_i, cross_i,".txt"), stringsAsFactors = FALSE)
    main_data.overlapped  = read.csv(paste0("overlapped/all/ALL-original-overlapped-", file_i, cross_i,".csv"), stringsAsFactors = FALSE)
    
    main_data.additional = main_data.3grams[,-ncol(main_data.3grams)]
    colnames(main_data.additional) = paste0("Word2Vec.", c(1:ncol(main_data.additional)))
    main_data.all = cbind.data.frame(main_data.additional, main_data.overlapped)
    
    write.csv(main_data.all, paste0("3grams/overlapped/3grams-overlapped-", file_i, cross_i,".csv"), row.names = FALSE)
    print(dim(main_data.all))
    print("-----")
  }
}