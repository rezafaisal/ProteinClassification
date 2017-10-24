setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

cross_num = 5
dividers = c(2,3,4,5)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
method_names = c("aac", "ctdd", "ctdc", "ctdt")
file_prefix_main = "-overlapped-divider-"

for(file_i in file_name_pattern){
  for(cross_i in 1:cross_num){
    if(exists("class_init")){
      rm("class_init")
    }
    
    if(exists("main_data.basket")){
      rm("main_data.basket")
    }
    
    for(divider_i in dividers){
      for(method_i in method_names){
        data_filename = paste0(method_i,file_prefix_main,divider_i,"-",file_i,cross_i,".txt")
        #print(data_filename)
        
        main_data = read.csv(paste0("overlapped/",data_filename), stringsAsFactors = FALSE)
        main_data.numeric = main_data[,-ncol(main_data)]
        colnames(main_data.numeric) = paste0(method_i,divider_i,".",colnames(main_data.numeric))
        
        #simpan data ke dalam basket
        if(!exists("main_data.basket")){
          assign("main_data.basket", main_data.numeric)
        } else {
          main_data.basket = cbind.data.frame(main_data.basket, main_data.numeric)
        }
        
        if(!exists("class_init")){
          assign("class_init", as.character(main_data$class))
        } else {
          if(!identical(class_init, as.character(main_data$class))){
            print("FALSE==========")
          }
        }
      }
    }
    
    main_data.basket = cbind.data.frame(main_data.basket, class_init)
    colnames(main_data.basket)[ncol(main_data.basket)] = "class"
    write.csv(main_data.basket, paste0("overlapped/result/ALL-overlapped-", file_i, cross_i,".csv"), row.names = FALSE)
    print(dim(main_data.basket))
    print("-----")
  }
}