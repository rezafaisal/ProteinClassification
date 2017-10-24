setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(kernlab)
library(caret)

cross_num = 10
print(paste("CV:", cross_num))
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")

dividers = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
#dividers = c(1,2)

for(divider_i in dividers){
  if(exists("main_data.prediction")){
    rm("main_data.prediction")
  }
  
  file_prefix = paste0("aac-overlapped-divider-",divider_i)
  main_data.file = paste0("200/", file_prefix, ".csv")
  main_data = read.csv(main_data.file, stringsAsFactors = FALSE)
  
  chloroplast = main_data[which(main_data$class == "chloroplast"),]
  cytoplasmic = main_data[which(main_data$class == "cytoplasmic"),]
  cytoskeleton = main_data[which(main_data$class == "cytoskeleton"),]
  er = main_data[which(main_data$class == "er"),]
  extracellular = main_data[which(main_data$class == "extracellular"),]
  golgi_apparatus = main_data[which(main_data$class == "golgi_apparatus"),]
  lysosomal = main_data[which(main_data$class == "lysosomal"),]
  mitochondrial = main_data[which(main_data$class == "mitochondrial"),]
  nuclear = main_data[which(main_data$class == "nuclear"),]
  peroxisomal = main_data[which(main_data$class == "peroxisomal"),]
  plasma_membrane = main_data[which(main_data$class == "plasma_membrane"),]
  vacuolar = main_data[which(main_data$class == "vacuolar"),]
  
  cross_chloroplast = floor(nrow(chloroplast)/cross_num)
  cross_cytoplasmic = floor(nrow(cytoplasmic)/cross_num)
  cross_cytoskeleton = floor(nrow(cytoskeleton)/cross_num)
  cross_er = floor(nrow(er)/cross_num)
  cross_extracellular = floor(nrow(extracellular)/cross_num)
  cross_golgi_apparatus = floor(nrow(golgi_apparatus)/cross_num)
  cross_lysosomal = floor(nrow(lysosomal)/cross_num)
  cross_mitochondrial = floor(nrow(mitochondrial)/cross_num)
  cross_nuclear = floor(nrow(nuclear)/cross_num)
  cross_peroxisomal = floor(nrow(peroxisomal)/cross_num)
  cross_plasma_membrane = floor(nrow(plasma_membrane)/cross_num)
  cross_vacuolar = floor(nrow(vacuolar)/cross_num)
  
  for(cross_i in 1:cross_num){
    start_chloroplast_i = cross_i
    start_cytoplasmic_i = cross_i
    start_cytoskeleton_i = cross_i
    start_er_i = cross_i
    start_extracellular_i = cross_i
    start_golgi_apparatus_i = cross_i
    start_lysosomal_i = cross_i
    start_mitochondrial_i = cross_i
    start_nuclear_i = cross_i
    start_peroxisomal_i = cross_i
    start_plasma_membrane_i = cross_i
    start_vacuolar_i = cross_i
    
    if(cross_i == 1){
      end_chloroplast_i = cross_i*cross_chloroplast
      end_cytoplasmic_i = cross_i*cross_cytoplasmic
      end_cytoskeleton_i = cross_i*cross_cytoskeleton
      end_er_i = cross_i*cross_er
      end_extracellular_i = cross_i*cross_extracellular
      end_golgi_apparatus_i = cross_i*cross_golgi_apparatus
      end_lysosomal_i = cross_i*cross_lysosomal
      end_mitochondrial_i = cross_i*cross_mitochondrial
      end_nuclear_i = cross_i*cross_nuclear
      end_peroxisomal_i = cross_i*cross_peroxisomal
      end_plasma_membrane_i = cross_i*cross_plasma_membrane
      end_vacuolar_i = cross_i*cross_vacuolar
    }
    
    if(cross_i > 1){
      start_chloroplast_i = end_chloroplast_i+1
      start_cytoplasmic_i = end_cytoplasmic_i+1
      start_cytoskeleton_i = end_cytoskeleton_i+1
      start_er_i = end_er_i+1
      start_extracellular_i = end_extracellular_i+1
      start_golgi_apparatus_i = end_golgi_apparatus_i+1
      start_lysosomal_i = end_lysosomal_i+1
      start_mitochondrial_i = end_mitochondrial_i+1
      start_nuclear_i = end_nuclear_i+1
      start_peroxisomal_i = end_peroxisomal_i+1
      start_plasma_membrane_i = end_plasma_membrane_i+1
      start_vacuolar_i = end_vacuolar_i+1
      
      end_chloroplast_i = cross_i*cross_chloroplast
      end_cytoplasmic_i = cross_i*cross_cytoplasmic
      end_cytoskeleton_i = cross_i*cross_cytoskeleton
      end_er_i = cross_i*cross_er
      end_extracellular_i = cross_i*cross_extracellular
      end_golgi_apparatus_i = cross_i*cross_golgi_apparatus
      end_lysosomal_i = cross_i*cross_lysosomal
      end_mitochondrial_i = cross_i*cross_mitochondrial
      end_nuclear_i = cross_i*cross_nuclear
      end_peroxisomal_i = cross_i*cross_peroxisomal
      end_plasma_membrane_i = cross_i*cross_plasma_membrane
      end_vacuolar_i = cross_i*cross_vacuolar
    }
    
    if(cross_i == cross_num){
      end_chloroplast_i = nrow(chloroplast)
      end_cytoplasmic_i = nrow(cytoplasmic)
      end_cytoskeleton_i = nrow(cytoskeleton)
      end_er_i = nrow(er)
      end_extracellular_i = nrow(extracellular)
      end_golgi_apparatus_i = nrow(golgi_apparatus)
      end_lysosomal_i = nrow(lysosomal)
      end_mitochondrial_i = nrow(mitochondrial)
      end_nuclear_i = nrow(nuclear)
      end_peroxisomal_i = nrow(peroxisomal)
      end_plasma_membrane_i = nrow(plasma_membrane)
      end_vacuolar_i = nrow(vacuolar)
    }
    
    main_data.test = rbind.data.frame(
                    chloroplast[c(start_chloroplast_i:end_chloroplast_i),], 
                    cytoplasmic[c(start_cytoplasmic_i:end_cytoplasmic_i),],
                    cytoskeleton[c(start_cytoskeleton_i:end_cytoskeleton_i),],
                    er[c(start_er_i:end_er_i),], 
                    extracellular[c(start_extracellular_i:end_extracellular_i),],
                    golgi_apparatus[c(start_golgi_apparatus_i:end_golgi_apparatus_i),], 
                    lysosomal[c(start_lysosomal_i:end_lysosomal_i),],
                    mitochondrial[c(start_mitochondrial_i:end_mitochondrial_i),], 
                    nuclear[c(start_nuclear_i:end_nuclear_i),],
                    peroxisomal[c(start_peroxisomal_i:end_peroxisomal_i),],
                    plasma_membrane[c(start_plasma_membrane_i:end_plasma_membrane_i),],
                    vacuolar[c(start_vacuolar_i:end_vacuolar_i),]
                     )
    main_data.train = rbind.data.frame(
                    chloroplast[-c(start_chloroplast_i:end_chloroplast_i),], 
                    cytoplasmic[-c(start_cytoplasmic_i:end_cytoplasmic_i),],
                    cytoskeleton[-c(start_cytoskeleton_i:end_cytoskeleton_i),],
                    er[-c(start_er_i:end_er_i),], 
                    extracellular[-c(start_extracellular_i:end_extracellular_i),],
                    golgi_apparatus[-c(start_golgi_apparatus_i:end_golgi_apparatus_i),], 
                    lysosomal[-c(start_lysosomal_i:end_lysosomal_i),],
                    mitochondrial[-c(start_mitochondrial_i:end_mitochondrial_i),], 
                    nuclear[-c(start_nuclear_i:end_nuclear_i),],
                    peroxisomal[-c(start_peroxisomal_i:end_peroxisomal_i),],
                    plasma_membrane[-c(start_plasma_membrane_i:end_plasma_membrane_i),],
                    vacuolar[-c(start_vacuolar_i:end_vacuolar_i),]
                  )
    
    main_data.test$class = as.factor(main_data.test$class)
    main_data.train$class = as.factor(main_data.train$class)
    
    #classification
    classification_model = ksvm(class~., main_data.train, type="spoc-svc")
    predict_result <- predict(classification_model, main_data.test[,-(ncol(main_data.test))])
    
    #prediction
    performance_data = cbind(as.character(predict_result), as.character(main_data.test[,(ncol(main_data.test))]))
    if(!exists("main_data.prediction")){
      assign("main_data.prediction", predict_result)
    } else {
      main_data.prediction = rbind.data.frame(main_data.prediction, predict_result)
    }
  }
  
  #performance
  print(paste0("Divider: ",divider_i,"=========================================================="))
  matrix.sens.spec = confusionMatrix(as.character(performance_data[,1]), as.character(performance_data[,2]))
  print(matrix.sens.spec$byClass[,c(5:7)])
}
