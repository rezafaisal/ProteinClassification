setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(randomForest)
library(kknn)
library(caret)

divider_j = 5
k_value = 5
cross_num=5
feature_count.init = 50
feature_count = 50
feature_count.max = 300
feature_increment = 50
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
dividers = c(2,3,4,5)
dividers = c(divider_j)

print(paste(Sys.time(), "ALL-original-overlapped Step: 50"))
#print(paste("Divider: ", divider_j))

if(exists("main_data.perf")){
  rm("main_data.perf")
}

if(exists("main_data.ranking")){
  rm("main_data.ranking")
}


file_prefix = paste0("ALL-original-overlapped-")

#classification - start
#klasifikasi akan dilakukan berdasarkan feature-feature yang diinginkan
#jumlah feature akan dimulai dari 2 sampai akhir kolom feature
#==========================================================================================
if(exists("main_data.prediction")){
  rm("main_data.prediction")
}

if(exists("main_data.performance")){
  rm("main_data.performance")
}

if(exists("main_data.class")){
  rm("main_data.class")
}


for(cross_i in 1:cross_num){
  feature_count = feature_count.init
  
  if(exists("main_data.train")){
    rm("main_data.train")
  }
  
  if(exists("main_data.test")){
    rm("main_data.test")
  }
  
  if(exists("main_data.class.temp")){
    rm("main_data.class.temp")
  }
  
  if(exists("main_data.prediction.temp")){
    rm("main_data.prediction.temp")
  }
  
  #mengumpulkan data training
  for(file_j in file_index[-cross_i]){
    for(file_name_i in file_name_pattern){
      main_data.train.file = paste0("overlapped/all/", file_prefix, file_name_i, file_j, ".csv")
      if(!exists("main_data.train")){
        assign("main_data.train", read.csv(main_data.train.file, stringsAsFactors = FALSE))
      } else {
        main_data.train = rbind.data.frame(main_data.train, read.csv(main_data.train.file, stringsAsFactors = FALSE))
      }
    }
  }
  #main_data.train = cbind.data.frame(main_data.train[,result.rf.features[1:feature_count]], main_data.train$class)
  colnames(main_data.train)[ncol(main_data.train)] = "class"
  main_data.train$class = as.factor(main_data.train$class)
  
  #mengumpulkan data testing
  for(file_name_i in file_name_pattern){
    main_data.test.file = paste0("overlapped/all/", file_prefix, file_name_i, cross_i, ".csv")
    if(!exists("main_data.test")){
      assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
    } else {
      main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
    }
  }
  #main_data.test = cbind.data.frame(main_data.test[,result.rf.features[1:feature_count]], main_data.test$class)
  colnames(main_data.test)[ncol(main_data.test)] = "class"
  main_data.test$class = as.factor(main_data.test$class)
  
  #feature selection - start
  #==========================================================================================
  main_data.train = na.omit(main_data.train)
  main_data.test = na.omit(main_data.test)
  
  result.rf <- randomForest(class ~ ., data=main_data.train, importance=TRUE, proximity=TRUE)
  result.rf.importance = as.data.frame(result.rf$importance[,(ncol(result.rf$importance)-1):ncol(result.rf$importance)])
  result.rf.sort = result.rf.importance[order(-result.rf.importance$MeanDecreaseAccuracy),]
  result.rf.features = rownames(result.rf.sort)
  
  #simpan features yang telah dirangking
  if(!exists("main_data.ranking")){
    assign("main_data.ranking", result.rf.features)
  } else {
    main_data.ranking = rbind(main_data.ranking, result.rf.features)
  }
  #==========================================================================================
  #feature selection - end
  
  feature_count.max = length(result.rf.features)
  while(feature_count < feature_count.max){
    print(feature_count)
    #membuat data training dan testing dengan feature sesuai urutan feature penting hasil dari feature selection - start
    #==========================================================================================
    main_data.train.temp = cbind.data.frame(main_data.train[,result.rf.features[1:feature_count]], main_data.train$class)
    colnames(main_data.train.temp)[ncol(main_data.train.temp)] = "class"
    main_data.train.temp$class = as.factor(main_data.train.temp$class)
    
    main_data.test.temp = cbind.data.frame(main_data.test[,result.rf.features[1:feature_count]], main_data.test$class)
    colnames(main_data.test.temp)[ncol(main_data.test.temp)] = "class"
    main_data.test.temp$class = as.factor(main_data.test.temp$class)
    #==========================================================================================
    
    #classification
    classification_model = kknn(class~., main_data.train.temp, main_data.test.temp[,-ncol(main_data.test.temp)], k = k_value, kernel = "triangular")
    predict_result <- fitted(classification_model)
    
    if(!exists("main_data.prediction.temp")){
      assign("main_data.prediction.temp", as.character(predict_result))
    } else {
      main_data.prediction.temp = rbind(main_data.prediction.temp, as.character(predict_result))
    }
    
    if(!exists("main_data.class.temp")){
      assign("main_data.class.temp", as.character(main_data.test.temp$class))
    } else {
      main_data.class.temp = rbind(main_data.class.temp, as.character(main_data.test.temp$class))
    }
    
    feature_count = feature_count + feature_increment
    if(feature_count > length(result.rf.features)){
      feature_count = length(result.rf.features)
    }
  }
  
  #mengumpulkan seluruh data testing
  if(!exists("main_data.class")){
    assign("main_data.class", main_data.class.temp)
  } else {
    main_data.class = cbind.data.frame(main_data.class, main_data.class.temp)
  }
  
  if(!exists("main_data.prediction")){
    assign("main_data.prediction", main_data.prediction.temp)
  } else {
    main_data.prediction = cbind(main_data.prediction, main_data.prediction.temp)
  }
  
  print("class")
  print(dim(main_data.class))
  
  print("prediction")
  print(dim(main_data.prediction))
  
  print(paste("cross:",cross_i,"======================================================="))
}

#main_data.class.all = rbind(cbind(main_data.class[1,]), cbind(main_data.class[2,]), cbind(main_data.class[3,]), cbind(main_data.class[4,]), cbind(main_data.class[5,]))
#main_data.class.all = na.omit(main_data.class.all)

for(predict_i in 1:nrow(main_data.prediction)){
  matrix.sens.spec = confusionMatrix(unlist(main_data.prediction[predict_i,]), unlist(main_data.class[predict_i,]))
  perf.temp = colSums(matrix.sens.spec$byClass[,c(5:7)], na.rm = TRUE)/(nrow(matrix.sens.spec$byClass[,c(5:7)]))
  print(colSums(matrix.sens.spec$byClass[,c(5:7)], na.rm = TRUE)/(nrow(matrix.sens.spec$byClass[,c(5:7)])))
  
  if(!exists("main_data.perf")){
    assign("main_data.perf", perf.temp)
  } else {
    main_data.perf = rbind.data.frame(main_data.perf, perf.temp)
  }
  print(paste(predict_i,"========================================="))
}
#==========================================================================================
#classification - end

colnames(main_data.perf) = c("Precision","Recall", "F1")
write.csv(main_data.perf, paste0("result/overlapped/kknn-cv-original-overlapped.50inc.csv"), row.names = FALSE, quote = FALSE)


colnames(main_data.ranking) = paste0("V", c(1:ncol(main_data.ranking)))
write.csv(main_data.ranking, paste0("result/overlapped/kknn-features-rangking-original-overlapped.50inc.csv"), row.names = FALSE, quote = FALSE)