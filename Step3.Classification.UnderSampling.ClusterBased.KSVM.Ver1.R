setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(kernlab)
library(caret)

cross_num = 5
samples_number = 120
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
#file_name_pattern = c("cytop", "plas")
class_name_us = c("chloroplast", "cytoplasmic", "extracellular", "mitochondrial", "nuclear", "plasma_membrane")
dividers = c(6,7,8,9,10)

for(divider_i in dividers){
  file_prefix = paste0("ctdd-divider-",divider_i,"-")
  
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
        main_data.train.read = read.csv(main_data.train.file, stringsAsFactors = FALSE)
        
        #cluster-based undersampling data training
        if(nrow(main_data.train.read) > samples_number){
          main_data.train.cluster = kmeans(main_data.train.read[,-ncol(main_data.train.read)], samples_number)
          main_data.train.cluster.data = as.data.frame(main_data.train.cluster$cluster)
          colnames(main_data.train.cluster.data)[1] = "cluster"
          
          if(exists("main_data.train.read.temp")){
            rm("main_data.train.read.temp")
          }
          
          for(cluster_i in 1:nrow(main_data.train.cluster$centers)){
            samples.temp = main_data.train.read[c(which(main_data.train.cluster.data$cluster == cluster_i)),]
            center.temp = main_data.train.cluster$centers[cluster_i,]
            
            if(nrow(samples.temp) > 1){
              #find nn
              jarak = as.matrix(dist(rbind(center.temp, samples.temp[,-ncol(samples.temp)])))
              jarak = sort(jarak[,1])
              samples.temp = main_data.train.read[c(names(jarak[2])),]
              
              if(!exists("main_data.train.read.temp")){
                assign("main_data.train.read.temp", samples.temp)
              } else {
                main_data.train.read.temp = rbind.data.frame(main_data.train.read.temp, samples.temp)
              }
            } else if (nrow(samples.temp) == 1) {
              if(!exists("main_data.train.read.temp")){
                assign("main_data.train.read.temp", samples.temp)
              } else {
                main_data.train.read.temp = rbind.data.frame(main_data.train.read.temp, samples.temp)
              }
            }
          }
        }
        
        main_data.train.read = main_data.train.read.temp
        if(!exists("main_data.train")){
          assign("main_data.train", main_data.train.read)
        } else {
          main_data.train = rbind.data.frame(main_data.train, main_data.train.read)
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
  print(paste0("Divider: ",divider_i,"=========================================================="))
  #colnames(performance_data) = c("predict", "actual")
  matrix.sens.spec = confusionMatrix(as.character(performance_data[,1]), as.character(performance_data[,2]))
  print(matrix.sens.spec$byClass[,c(5:7)])
  #print(matrix.sens.spec)
  #print(matrix.sens.spec$byClass)
  performance_result = cbind.data.frame(rownames(matrix.sens.spec$byClass), matrix.sens.spec$byClass[,c(5:7)])
  colnames(performance_result)[1] = "Class"
  write.csv(performance_result, paste0("result/",file_prefix,"result-",divider_i,".txt"), row.names = FALSE, quote = FALSE)
}
