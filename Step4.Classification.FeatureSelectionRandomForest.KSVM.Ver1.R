setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(kernlab)
library(caret)
library(Boruta)

cross_num = 5
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
dividers = c(2,3,4,5)
#dividers = c(4,5)

for(divider_i in dividers){
  file_prefix = paste0("ctdd-ctdc-ctdt-aac-divider-",divider_i,"-")
  
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
    #======================================================================
    for(file_i in file_index[-cross_i]){
      for(file_name_i in file_name_pattern){
        main_data.train.file = paste0("combination/", file_prefix, file_name_i, file_i, ".txt")
        if(!exists("main_data.train")){
          assign("main_data.train", read.csv(main_data.train.file, stringsAsFactors = FALSE))
        } else {
          main_data.train = rbind.data.frame(main_data.train, read.csv(main_data.train.file, stringsAsFactors = FALSE))
        }
      }
    }
    main_data.train$class = as.factor(main_data.train$class)
    
    #mengumpulkan data testing
    #======================================================================
    for(file_name_i in file_name_pattern){
      main_data.test.file = paste0("combination/", file_prefix, file_name_i, cross_i, ".txt")
      if(!exists("main_data.test")){
        assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
      } else {
        main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
      }
    }
    main_data.test$class = as.factor(main_data.test$class)
    
    #feature selection using boruta on training data
    #======================================================================
    main_data.train = na.omit(main_data.train)
    boruta.train <- Boruta(class~., data = main_data.train, doTrace = 2)
    boruta.train.stat = attStats(boruta.train)
    boruta.train.features = rownames(boruta.train.stat[which(boruta.train.stat$decision == "Confirmed"),])
    main_data.train = cbind.data.frame(main_data.train[,boruta.train.features], main_data.train[,ncol(main_data.train)])
    main_data.test= cbind.data.frame(main_data.test[,boruta.train.features], main_data.test[,ncol(main_data.test)])
    colnames(main_data.train)[ncol(main_data.train)] = "class"
    colnames(main_data.test)[ncol(main_data.train)] = "class"
    
    #classification
    #======================================================================
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
  print(paste0("Divider: ",divider_i,"=========================================================="))
  #colnames(performance_data) = c("predict", "actual")
  matrix.sens.spec = confusionMatrix(as.character(performance_data[,1]), as.character(performance_data[,2]))
  print(matrix.sens.spec$byClass[,c(5:7)])
  #print(matrix.sens.spec)
  #print(matrix.sens.spec$byClass[c(1,2,5,6,7,11)])
  performance_result = cbind.data.frame(rownames(matrix.sens.spec$byClass), matrix.sens.spec$byClass[,c(5:7)])
  colnames(performance_result)[1] = "Class"
  write.csv(performance_result, paste0("result/",file_prefix,"result-",divider_i,".txt"), row.names = FALSE, quote = FALSE)
}
