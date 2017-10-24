setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(kernlab)
library(caret)

cross_num = 5
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
#file_name_pattern = c("cytop", "plas")
file_prefix = "ctdd-divider-3-"

if(exists("main_data.prediction")){
  rm("main_data.prediction")
}

for(cross_i in 1:cross_num){
  if(exists("main_data.train")){
    rm("main_data.train")
  }
  
  if(exists("main_data.test")){
    rm("main_data.test")
  }
  
  #mengumpulkan data training
  for(file_i in file_index[-cross_i]){
    for(file_name_i in file_name_pattern){
      main_data.train.file = paste0("segmented/", file_prefix, file_name_i, file_i, ".txt")
      if(!exists("main_data.train")){
        assign("main_data.train", read.csv(main_data.train.file, stringsAsFactors = FALSE))
      } else {
        main_data.train = rbind.data.frame(main_data.train, read.csv(main_data.train.file, stringsAsFactors = FALSE))
      }
    }
  }
  main_data.train$class = as.factor(main_data.train$class)
  
  #mengumpulkan data testing
  for(file_name_i in file_name_pattern){
    main_data.test.file = paste0("segmented/", file_prefix, file_name_i, cross_i, ".txt")
    if(!exists("main_data.test")){
      assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
    } else {
      main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
    }
  }
  main_data.test$class = as.factor(main_data.test$class)
  
  classification_model = ksvm(class~., main_data.train, type = "spoc-svc")
  predict_result <- predict(classification_model, main_data.test[,-(ncol(main_data.test))])
  
  performance_data = cbind(as.character(predict_result), as.character(main_data.test[,(ncol(main_data.test))]))
  
  if(!exists("main_data.prediction")){
    assign("main_data.prediction", predict_result)
  } else {
    main_data.prediction = rbind.data.frame(main_data.prediction, predict_result)
  }
}

#performance
#colnames(performance_data) = c("predict", "actual")
matrix.sens.spec = confusionMatrix(as.character(performance_data[,1]), as.character(performance_data[,2]))
print(matrix.sens.spec$byClass[,c(5:7)])
#print(matrix.sens.spec)
#print(matrix.sens.spec$byClass)
performance_result = cbind.data.frame(rownames(matrix.sens.spec$byClass), matrix.sens.spec$byClass[,c(5:7)])
colnames(performance_result)[1] = "Class"
write.csv(performance_result, paste0("result/",file_prefix,"result.txt"), row.names = FALSE, quote = FALSE)

