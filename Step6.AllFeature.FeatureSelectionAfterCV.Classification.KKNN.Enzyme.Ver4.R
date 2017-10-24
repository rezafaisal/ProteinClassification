setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(randomForest)
library(kknn)
library(caret)

k_value = 5
cross_num=10
feature_count.init = 10
feature_count = 10
feature_count.max = 300
feature_increment = 10
positive_label = "enzyme"
negative_label = "notenzyme"

main_data = read.csv("enzymes/ctdd-ctdc-ctdt-aac-divider-ALL-enzyme-all.csv", stringsAsFactors = FALSE)
main_data = na.omit(main_data)
main_data.positive = main_data[which(main_data$class == positive_label),]
main_data.negative = main_data[which(main_data$class == negative_label),]

cross_positive = floor(nrow(main_data.positive)/cross_num)
cross_negative = floor(nrow(main_data.negative)/cross_num)

if(exists("main_data.perf")){
  rm("main_data.perf")
}

if(exists("main_data.ranking")){
  rm("main_data.ranking")
}

if(exists("main_data.prediction")){
  rm("main_data.prediction")
}

if(exists("main_data.performance")){
  rm("main_data.performance")
}

if(exists("main_data.class")){
  rm("main_data.class")
}

#read

for(cross_i in 1:cross_num){
  feature_count = feature_count.init
  
  if(exists("main_data.class.temp")){
    rm("main_data.class.temp")
  }
  
  if(exists("main_data.prediction.temp")){
    rm("main_data.prediction.temp")
  }
  
  start_positive_i = cross_i
  start_negative_i = cross_i
  
  if(cross_i == 1){
    end_positive_i = cross_i*cross_positive
    end_negative_i = cross_i*cross_negative
  }
  
  if(cross_i > 1){
    start_positive_i = end_positive_i+1
    start_negative_i = end_negative_i+1
    
    end_positive_i = (cross_i)*cross_positive
    end_negative_i = (cross_i)*cross_negative
  }
  
  if(cross_i == cross_num){
    end_positive_i = nrow(main_data.positive)
    end_negative_i = nrow(main_data.negative)
  }
  
  main_data.test = rbind.data.frame(main_data.positive[c(start_positive_i:end_positive_i),], main_data.negative[c(start_negative_i:end_negative_i),])
  main_data.train = rbind.data.frame(main_data.positive[-c(start_positive_i:end_positive_i),], main_data.negative[-c(start_negative_i:end_negative_i),])
  
  main_data.test$class = as.factor(main_data.test$class)
  main_data.train$class = as.factor(main_data.train$class)
  
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
  
  feature_count.max = length(result.rf.features)
  #==========================================================================================
  #feature selection - end
  
  while(feature_count < feature_count.max){
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
      assign("main_data.class.temp", as.character(main_data.test.temp[,ncol(main_data.test.temp)]))
    } else {
      main_data.class.temp = rbind(main_data.class.temp, as.character(main_data.test.temp[,ncol(main_data.test.temp)]))
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
  print("main_data.class.temp")
  print(length(main_data.class.temp))
  
  if(!exists("main_data.prediction")){
    assign("main_data.prediction", main_data.prediction.temp)
  } else {
    main_data.prediction = cbind(main_data.prediction, main_data.prediction.temp)
  }
  print("main_data.prediction.temp")
  print(length(main_data.prediction.temp))
  
  print("class")
  print(dim(main_data.class))
  
  print("prediction")
  print(dim(main_data.prediction))
  
  print(paste("cross:",cross_i,"======================================================="))
}

for(predict_i in 1:nrow(main_data.prediction)){
  matrix.sens.spec = confusionMatrix(unlist(main_data.prediction[predict_i,]), unlist(main_data.class[predict_i,]))
  #perf.temp = colSums(matrix.sens.spec$byClass[,c(5:7)], na.rm = TRUE)/(nrow(matrix.sens.spec$byClass[,c(5:7)]))
  perf.temp = cbind.data.frame(rbind(matrix.sens.spec$byClass[c(1,2,7,11)]),matrix.sens.spec$overall[1])
  colnames(perf.temp)[ncol(perf.temp)] = "Accuracy"
  rownames(perf.temp) = predict_i
  print(perf.temp)
  
  if(!exists("main_data.perf")){
    assign("main_data.perf", perf.temp)
  } else {
    main_data.perf = rbind.data.frame(main_data.perf, perf.temp)
  }
  #print(paste(predict_i,"========================================="))
}

#colnames(main_data.perf) = c("Precision","Recall", "F1")
write.csv(main_data.perf, paste0("enzymes/result/kknn-cv-allcombination.fs.divider-ALL-counter10.csv"), row.names = FALSE, quote = FALSE)

colnames(main_data.ranking) = paste0("V", c(1:ncol(main_data.ranking)))
write.csv(main_data.ranking, paste0("enzymes/result/kknn-10step-features-rangking.csv"), row.names = FALSE, quote = FALSE)
