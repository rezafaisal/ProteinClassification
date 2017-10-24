setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(randomForest)
library(kernlab)
library(caret)

cross_num=5
feature_count = 10
feature_increment = 10
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
dividers = c(1,2,3,4,5)
dividers = c(1)

for(divider_i in dividers){
  file_prefix = paste0("ctdd-ctdc-ctdt-aac-divider-",divider_i,"-")
  
  #all data - start
  #==========================================================================================
  if(exists("main_data.all")){
    rm("main_data.all")
  }
  
  for(file_i in 1:cross_num){
    #menyimpan data pada main_data.all
    for(file_name_i in file_name_pattern){
      main_data.temp.file = paste0("combination/", file_prefix, file_name_i, file_i, ".txt")
      main_data.temp = read.csv(main_data.temp.file, stringsAsFactors = FALSE)
      
      #menyimpan data pada main_data.all
      if(!exists("main_data.all")){
        assign("main_data.all", main_data.temp)
      } else {
        main_data.all = rbind.data.frame(main_data.all, main_data.temp)
      }
    }
  }
  
  main_data.all = na.omit(main_data.all)
  main_data.all$class = as.factor(main_data.all$class)
  #==========================================================================================
  #all data - end
  
  
  #feature selection - start
  #==========================================================================================
  result.rf <- randomForest(class ~ ., data=main_data.all, importance=TRUE, proximity=TRUE)
  result.rf.importance = as.data.frame(result.rf$importance[,(ncol(result.rf$importance)-1):ncol(result.rf$importance)])
  result.rf.sort = result.rf.importance[order(-result.rf.importance$MeanDecreaseAccuracy),]
  result.rf.features = rownames(result.rf.sort)
  #==========================================================================================
  #feature selection - end
  
  
  #classification - start
  #klasifikasi akan dilakukan berdasarkan feature-feature yang diinginkan
  #jumlah feature akan dimulai dari 2 sampai akhir kolom feature
  #==========================================================================================
  if(exists("main_data.prediction")){
    rm("main_data.prediction")
  }
  
  while(feature_count <= length(result.rf.features)){
    for(cross_i in 1:cross_num){
      if(exists("main_data.train")){
        rm("main_data.train")
      }
      
      if(exists("main_data.test")){
        rm("main_data.test")
      }
      
      #mengumpulkan data training
      for(file_j in file_index[-cross_i]){
        for(file_name_i in file_name_pattern){
          main_data.train.file = paste0("combination/", file_prefix, file_name_i, file_j, ".txt")
          if(!exists("main_data.train")){
            assign("main_data.train", read.csv(main_data.train.file, stringsAsFactors = FALSE))
          } else {
            main_data.train = rbind.data.frame(main_data.train, read.csv(main_data.train.file, stringsAsFactors = FALSE))
          }
        }
      }
      main_data.train = cbind.data.frame(main_data.train[,c(1:feature_count)], main_data.train$class)
      colnames(main_data.train)[ncol(main_data.train)] = "class"
      main_data.train$class = as.factor(main_data.train$class)
      
      #mengumpulkan data testing
      for(file_name_i in file_name_pattern){
        main_data.test.file = paste0("combination/", file_prefix, file_name_i, cross_i, ".txt")
        if(!exists("main_data.test")){
          assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
        } else {
          main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
        }
      }
      main_data.test = cbind.data.frame(main_data.test[,c(1:feature_count)], main_data.test$class)
      colnames(main_data.test)[ncol(main_data.test)] = "class"
      main_data.test$class = as.factor(main_data.test$class)
      
      #classification
      classification_model = ksvm(class~., main_data.train, type = "spoc-svc")
      predict_result <- predict(classification_model, main_data.test[,-(ncol(main_data.test))])
      performance_data = cbind(as.character(predict_result), as.character(main_data.test[,(ncol(main_data.test))]))
      
      if(!exists("main_data.prediction")){
        assign("main_data.prediction", predict_result)
      } else {
        main_data.prediction = rbind.data.frame(main_data.prediction, predict_result)
      }
    }
    
    print(paste0("Feature: ",feature_count,"=========================================================="))
    matrix.sens.spec = confusionMatrix(as.character(performance_data[,1]), as.character(performance_data[,2]))
    print(matrix.sens.spec$byClass[,c(5:7)])
    
    feature_count = feature_count + feature_increment
    if(feature_count > length(result.rf.features)){
      feature_count = length(result.rf.features)
    }
  }
  #==========================================================================================
  #classification - end
}