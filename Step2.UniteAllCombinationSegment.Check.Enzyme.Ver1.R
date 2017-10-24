
setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

cross_num = 5
dividers = c(1,2,3,4,5)
file_name_pattern = c("enzyme-all")
file_prefix_main = "ctdd-ctdc-ctdt-aac-divider-"

for(file_i in file_name_pattern){
    if(exists("class_value.init")){
      rm("class_value.init")
    }
    is.class.same = FALSE
    
    for(divider_i in dividers){
      data_filename = paste0(file_prefix_main,divider_i,"-",file_i,".csv")
      main_data = read.csv(paste0("enzymes/",data_filename), stringsAsFactors = FALSE)
      print(data_filename)
      
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
        if(is.class.same == FALSE){
          print(is.class.same)
          print("==========================")
        }
      }
    }
}