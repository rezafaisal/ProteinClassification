setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
#setwd("C:/Users/M Reza Faisal/Documents/OneDriveBussiness/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(randomForest)
library(kknn)
library(caret)

divider_j = 5
k_value = 5
cross_num=5
feature_count.init = 10
feature_count = 10
feature_count.max = 1019
feature_increment = 10
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
dividers = c(2,3,4,5)
dividers = c(divider_j)

main_data.prediction.filename = "main_data.prediction.ver2.10inc.csv"
main_data.class.filename = "main_data.class.ver2.10inc.csv"
features.existing.filename = "kknn-features-rangking-original-overlapped.10inc.csv"

#baca features
features.existing = read.csv(paste0("result/overlapped/", features.existing.filename), stringsAsFactors = FALSE)

print(paste(Sys.time(), "ALL-original-overlapped Step: 10"))
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
  
  #menggunakan features yang telah ada - start
  #==========================================================================================
  main_data.train = na.omit(main_data.train)
  main_data.test = na.omit(main_data.test)
  result.rf.features = unlist(features.existing[cross_i,])
  #==========================================================================================
  #menggunakan features yang telah ada - end
  
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

#write result
#jika diperlukan kembali dikemudian hari
write.csv(main_data.prediction, paste0("result/perf/", main_data.prediction.filename), row.names = FALSE)
write.csv(main_data.class, paste0("result/perf/", main_data.class.filename), row.names = FALSE)

#read data jika diperlukan
main_data.prediction = read.csv(paste0("result/perf/", main_data.prediction.filename), stringsAsFactors = FALSE)
main_data.class = read.csv(paste0("result/perf/", main_data.class.filename), stringsAsFactors = FALSE)

#menghitung akurasi
for(predict_i in 1:nrow(main_data.prediction)){
  prediction.data = cbind(unlist(main_data.prediction[predict_i,]), as.character(unlist(main_data.class[predict_i,])))
  prediction.data = as.data.frame(prediction.data)
  colnames(prediction.data) = c("predict","class")
  
  #total akurasi (total akurasi = acc dari confusionMatrix)
  is_prediction_true.count = 0
  for(prediction.data_i in 1:nrow(prediction.data)){
    if(as.character(prediction.data[prediction.data_i, 1]) == as.character(prediction.data[prediction.data_i, 2])){
      is_prediction_true.count = is_prediction_true.count + 1
    }
  }
  total_acc = is_prediction_true.count/nrow(prediction.data)
  print(paste("Total Acc:", total_acc))
  # matrix.sens.spec = confusionMatrix(as.character(prediction.data$predict), as.character(prediction.data$class))
  #print(paste("Total Acc:",matrix.sens.spec$overall[1]))
  
  #lokal akurasi
  #menghitung akurasi setiap class kemudian dijumlahkan
  #kemudian dibagi dengan jumlah class
  local.acc.temp = 0
  prediction.data.classes = as.character(as.data.frame(table(prediction.data$class))$Var1)
  for(prediction.data.class in prediction.data.classes){
    prediction.data.temp = prediction.data[which(prediction.data$class == prediction.data.class),]
    
    is_prediction_true.count = 0
    for(prediction.data_i in 1:nrow(prediction.data.temp)){
      if(as.character(prediction.data.temp[prediction.data_i, 1]) == as.character(prediction.data.temp[prediction.data_i, 2])){
        is_prediction_true.count = is_prediction_true.count + 1
      }
    }
    local_acc = is_prediction_true.count/nrow(prediction.data.temp)
    
    local.acc.temp = local.acc.temp + local_acc
    #print(local_acc)
  }
  
  local.acc = local.acc.temp/length(prediction.data.classes)
  print(paste("Local Acc:", local.acc))
  print(paste(predict_i,"=================="))
  
  perf.temp = c(total_acc, local.acc)
  if(!exists("main_data.perf")){
    assign("main_data.perf", perf.temp)
  } else {
    main_data.perf = rbind.data.frame(main_data.perf, perf.temp)
  }
}




# for(predict_i in 1:nrow(main_data.prediction)){
#   matrix.sens.spec = confusionMatrix(unlist(main_data.prediction[predict_i,]), unlist(main_data.class[predict_i,]))
#   perf.temp = colSums(matrix.sens.spec$byClass[,c(5:7)], na.rm = TRUE)/(nrow(matrix.sens.spec$byClass[,c(5:7)]))
#   print(colSums(matrix.sens.spec$byClass[,c(5:7)], na.rm = TRUE)/(nrow(matrix.sens.spec$byClass[,c(5:7)])))
#   
#   if(!exists("main_data.perf")){
#     assign("main_data.perf", perf.temp)
#   } else {
#     main_data.perf = rbind.data.frame(main_data.perf, perf.temp)
#   }
#   print(paste(predict_i,"========================================="))
# }
#==========================================================================================
#classification - end

colnames(main_data.perf) = c("TotalAcc", "LocalAcc")
write.csv(main_data.perf, paste0("result/overlapped/kknn-cv-original-overlapped.10inc.accuracy.csv"), row.names = FALSE, quote = FALSE)


#colnames(main_data.ranking) = paste0("V", c(1:ncol(main_data.ranking)))
#write.csv(main_data.ranking, paste0("result/overlapped/kknn-features-rangking-original-overlapped.1inc.1013-1019.ver4.csv"), row.names = FALSE, quote = FALSE)