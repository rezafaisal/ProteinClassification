setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

cross_num = 5
dividers = c(1, 2,3,4,5)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
file_prefix_main = "ctdd-ctdc-ctdt-aac-divider-"

for(file_i in file_name_pattern){
  for(cross_i in 1:cross_num){
    is.class.same = FALSE
    
    if(exists("class_value.init")){
      rm("class_value.init")
    }
    
    if(exists("main_data.basket")){
      rm("main_data.basket")
    }
    
    
    for(divider_i in dividers){
      #read current file
      data_filename = paste0(file_prefix_main,divider_i,"-",file_i,cross_i,".txt")
      main_data = read.csv(paste0("combination/",data_filename), stringsAsFactors = FALSE)
      print(data_filename)
      
      #tambahkan nama colom dengan divider
      #untuk menandai colom berasal dari divider ke berapa
      main_data.numeric = main_data[,-ncol(main_data)]
      colnames(main_data.numeric) = paste0(divider_i,"-",colnames(main_data.numeric))
        
      #simpan data ke dalam basket
      if(!exists("main_data.basket")){
        assign("main_data.basket", main_data.numeric)
      } else {
        main_data.basket = cbind.data.frame(main_data.basket, main_data.numeric)
      }
      
      if(!exists("class_value.init")){
        assign("class_value.init", main_data$class)
      } else {
        if(identical(class_value.init, main_data$class)){
          is.class.same = TRUE
        } else {
          is.class.same = FALSE
        }
      }
      
      if(divider_i == 5){
        main_data.basket = cbind.data.frame(main_data.basket, class_value.init)
        colnames(main_data.basket)[ncol(main_data.basket)] = "class"
        write.csv(main_data.basket, paste0("combination-all/", file_prefix_main,"ALL-",file_i,cross_i,".txt"), row.names = FALSE)
        
        if(is.class.same == FALSE){
          print(is.class.same)
          print("==========================")
        }
      }
    }
    print("-----")
  }
}