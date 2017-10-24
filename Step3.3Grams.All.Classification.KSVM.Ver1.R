setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
#setwd("C:/Users/M Reza Faisal/Documents/OneDriveBussiness/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(kernlab)
library(caret)

k_value = 5
cross_num=5
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")

main_data.prediction.filename = "main_data.prediction.3grams-all.ksvm.csv"
main_data.class.filename = "main_data.class.3grams-all.ksvm.csv"


print(paste(Sys.time(), "word2vec feature - all - ksvm"))
#print(paste("Divider: ", divider_j))

if(exists("main_data.perf")){
  rm("main_data.perf")
}



file_prefix = paste0("numerical_all.")

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
      main_data.train.file = paste0("3grams/numerical_all/", file_prefix, file_name_i, file_j, ".txt")
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
    main_data.test.file = paste0("3grams/numerical_all/", file_prefix, file_name_i, cross_i, ".txt")
    if(!exists("main_data.test")){
      assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
    } else {
      main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
    }
  }
  #main_data.test = cbind.data.frame(main_data.test[,result.rf.features[1:feature_count]], main_data.test$class)
  colnames(main_data.test)[ncol(main_data.test)] = "class"
  main_data.test$class = as.factor(main_data.test$class)
  
  #classification
  classification_model = ksvm(class~., main_data.train, type = "spoc-svc")
  predict_result <- predict(classification_model, main_data.test[,-(ncol(main_data.test))])
  
  predict_result.temp = cbind(as.character(predict_result), as.character(main_data.test$class))
  if(!exists("main_data.prediction")){
    assign("main_data.prediction", predict_result.temp)
  } else {
    main_data.prediction = rbind(main_data.prediction, predict_result.temp)
  }
}

#write result
#jika diperlukan kembali dikemudian hari
colnames(main_data.prediction) = c("prediction", "class")
write.csv(main_data.prediction, paste0("3grams/perf/", main_data.prediction.filename), row.names = FALSE)

#read data jika diperlukan
main_data.prediction = read.csv(paste0("3grams/perf/", main_data.prediction.filename), stringsAsFactors = FALSE)

#menghitung akurasi
prediction.data = as.data.frame(main_data.prediction)
is_prediction_true.count = 0
for(prediction.data_i in 1:nrow(prediction.data)){
  if(as.character(prediction.data[prediction.data_i, 1]) == as.character(prediction.data[prediction.data_i, 2])){
    is_prediction_true.count = is_prediction_true.count + 1
  }
}
total_acc = is_prediction_true.count/nrow(prediction.data)
print(paste("Total Acc:", total_acc))

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
}

local.acc = local.acc.temp/length(prediction.data.classes)
print(paste("Local Acc:", local.acc))

matrix.sens.spec = confusionMatrix(as.character(prediction.data$predict), as.character(prediction.data$class))
print(matrix.sens.spec$byClass[,c(5:7)])
