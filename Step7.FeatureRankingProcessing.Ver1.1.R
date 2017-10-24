setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

feature_rangking.filename = "kknn-features-rangking-3grams-overlapped.50inc.accuracy.csv"

main_data = read.csv(paste0("3grams/result/", feature_rangking.filename), stringsAsFactors = FALSE)

key_aac1 = c("A", "R", "N", "D", "C", "E", "Q", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
key_aac2 = c("aac2", "aac3", "aac4", "aac5")

key_ctdc1 = c("hydrophobicity", "normwaalsvolume", "polarity", "polarizability", "charge", "secondarystruct", "solventaccess")
key_ctdc2 = c("ctdc2", "ctdc3", "ctdc4", "ctdc5")

key_ctdd1 = c("G1", "G2", "G3")
key_ctdd2 = c("ctdd2", "ctdd3", "ctdd4", "ctdd5")

key_ctdt1 = c("Tr1221", "Tr1331", "Tr2332")
key_ctdt2 = c("ctdt2", "ctdt3", "ctdt4", "ctdt5")

key_w2v = c("Word2Vec")

cross_num = 5
feature_max = 1350

if(exists("feature.result")){
  rm("feature.result")
}

if(exists("feature.result.acc")){
  rm("feature.result.acc")
}

if(exists("feature.result.ctdc")){
  rm("feature.result.ctdc")
}

if(exists("feature.result.ctdd")){
  rm("feature.result.ctdd")
}

if(exists("feature.result.ctdt")){
  rm("feature.result.ctdt")
}

for(cross_i in 1:cross_num){
  main_data.feature = main_data[cross_i, c(1:feature_max)]
  
  count_aac = 0
  count_aac1 = 0
  count_aac2 = 0
  count_aac3 = 0
  count_aac4 = 0
  count_aac5 = 0
  
  count_ctdc = 0
  count_ctdc1 = 0
  count_ctdc2 = 0
  count_ctdc3 = 0
  count_ctdc4 = 0
  count_ctdc5 = 0
  
  count_ctdd = 0
  count_ctdd1 = 0
  count_ctdd2 = 0
  count_ctdd3 = 0
  count_ctdd4 = 0
  count_ctdd5 = 0
  
  count_ctdt = 0
  count_ctdt1 = 0
  count_ctdt2 = 0
  count_ctdt3 = 0
  count_ctdt4 = 0
  count_ctdt5 = 0
  
  count_w2v = 0
  
  for(feature_i in main_data.feature){
    is_show = TRUE
    
    feature_type = strsplit(feature_i, "[.]")[[1]][1]
    feature_type.2 = strsplit(feature_i, "[.]")[[1]][2]
    
    if(is.element(feature_type, key_aac1) || is.element(feature_type, key_aac2)){
      count_aac = count_aac + 1
      
      if(is.element(feature_type, key_aac1)){
        count_aac1 = count_aac1 + 1
      } else if(is.element(feature_type, key_aac2)){
        feature_type.segment = as.numeric(substr(feature_type, nchar(feature_type), nchar(feature_type)))
        
        if(feature_type.segment == 2){
          count_aac2 = count_aac2 + 1
        } else if(feature_type.segment == 3){
          count_aac3 = count_aac3 + 1
        } else if(feature_type.segment == 4){
          count_aac4 = count_aac4 + 1
        } else if(feature_type.segment == 5){
          count_aac5 = count_aac5 + 1
        } 
      }
      
      is_show = FALSE
    } 
    else if(is.element(feature_type, key_ctdc1) || is.element(feature_type, key_ctdc2)){
      count_ctdc = count_ctdc + 1
      
      if(is.element(feature_type, key_ctdc1)){
        count_ctdc1 = count_ctdc1 + 1
      } else if(is.element(feature_type, key_ctdc2)){
        feature_type.segment = as.numeric(substr(feature_type, nchar(feature_type), nchar(feature_type)))
        
        if(feature_type.segment == 2){
          count_ctdc2 = count_ctdc2 + 1
        } else if(feature_type.segment == 3){
          count_ctdc3 = count_ctdc3 + 1
        } else if(feature_type.segment == 4){
          count_ctdc4 = count_ctdc4 + 1
        } else if(feature_type.segment == 5){
          count_ctdc5 = count_ctdc5 + 1
        }
      }
      
      is_show = FALSE
    } 
    else if(is.element(feature_type, key_ctdd1) || is.element(feature_type, key_ctdd2)){
      count_ctdd = count_ctdd + 1
      
      if(is.element(feature_type, key_ctdd2)){
        feature_type.segment = as.numeric(substr(feature_type, nchar(feature_type), nchar(feature_type)))
        
        if(feature_type.segment == 2){
          count_ctdd2 = count_ctdd2 + 1
        } else if(feature_type.segment == 3){
          count_ctdd3 = count_ctdd3 + 1
        } else if(feature_type.segment == 4){
          count_ctdd4 = count_ctdd4 + 1
        } else if(feature_type.segment == 5){
          count_ctdd5 = count_ctdd5 + 1
        }
      }
      
      is_show = FALSE
    }
    else if(is.element(feature_type, key_ctdt1) || is.element(feature_type, key_ctdt2)){
      count_ctdt = count_ctdt + 1
      
      if(is.element(feature_type, key_ctdt2)){
        feature_type.segment = as.numeric(substr(feature_type, nchar(feature_type), nchar(feature_type)))
        
        if(feature_type.segment == 2){
          count_ctdt2 = count_ctdt2 + 1
        } else if(feature_type.segment == 3){
          count_ctdt3 = count_ctdt3 + 1
        } else if(feature_type.segment == 4){
          count_ctdt4 = count_ctdt4 + 1
        } else if(feature_type.segment == 5){
          count_ctdt5 = count_ctdt5 + 1
        }
      }
      
      is_show = FALSE
    }
    else if (is.element(feature_type, key_w2v)){
      count_w2v = count_w2v + 1
      is_show = FALSE
    } 
    
    if (is.element(feature_type.2, key_ctdd1)){
      count_ctdd = count_ctdd + 1
      count_ctdd1 = count_ctdd1 + 1
      
      is_show = FALSE
    } else if(is.element(feature_type.2, key_ctdt1)){
      count_ctdt = count_ctdt + 1
      count_ctdt1 = count_ctdt1 + 1
      
      is_show = FALSE
    }
    
    if(is_show){
      print(feature_i)
    }
  }
  
  if(!exists("feature.result")){
    assign("feature.result", cbind(count_aac, count_ctdc, count_ctdd, count_ctdt, count_w2v, (count_aac + count_ctdc + count_ctdd + count_ctdt + count_w2v)))
  } else {
    feature.result = rbind(feature.result, cbind(count_aac, count_ctdc, count_ctdd, count_ctdt, count_w2v, (count_aac + count_ctdc + count_ctdd + count_ctdt + count_w2v)))
  }
  
  if(!exists("feature.result.acc")){
    assign("feature.result.acc", cbind(count_aac1, count_aac2, count_aac3, count_aac4, count_aac5, (count_aac1 + count_aac2 + count_aac3 + count_aac4 + count_aac5)))
  } else {
    feature.result.acc = rbind(feature.result.acc, cbind(count_aac1, count_aac2, count_aac3, count_aac4, count_aac5, (count_aac1 + count_aac2 + count_aac3 + count_aac4 + count_aac5)))
  }
  
  if(!exists("feature.result.ctdc")){
    assign("feature.result.ctdc", cbind(count_ctdc1, count_ctdc2, count_ctdc3, count_ctdc4, count_ctdc5, (count_ctdc1 + count_ctdc2 + count_ctdc3 + count_ctdc4 + count_ctdc5)))
  } else {
    feature.result.ctdc = rbind(feature.result.ctdc, cbind(count_ctdc1, count_ctdc2, count_ctdc3, count_ctdc4, count_ctdc5, (count_ctdc1 + count_ctdc2 + count_ctdc3 + count_ctdc4 + count_ctdc5)))
  }
  
  if(!exists("feature.result.ctdd")){
    assign("feature.result.ctdd", cbind(count_ctdd1, count_ctdd2, count_ctdd3, count_ctdd4, count_ctdd5, (count_ctdd1 + count_ctdd2 + count_ctdd3 + count_ctdd4 + count_ctdd5)))
  } else {
    feature.result.ctdd = rbind(feature.result.ctdd, cbind(count_ctdd1, count_ctdd2, count_ctdd3, count_ctdd4, count_ctdd5, (count_ctdd1 + count_ctdd2 + count_ctdd3 + count_ctdd4 + count_ctdd5)))
  }
  
  if(!exists("feature.result.ctdt")){
    assign("feature.result.ctdt", cbind(count_ctdt1, count_ctdt2, count_ctdt3, count_ctdt4, count_ctdt5, (count_ctdt1 + count_ctdt2 + count_ctdt3 + count_ctdt4 + count_ctdt5)))
  } else {
    feature.result.ctdt = rbind(feature.result.ctdt, cbind(count_ctdt1, count_ctdt2, count_ctdt3, count_ctdt4, count_ctdt5, (count_ctdt1 + count_ctdt2 + count_ctdt3 + count_ctdt4 + count_ctdt5)))
  }
}

colnames(feature.result) = c("aac", "ctdc", "ctdd", "ctdt", "word2vec", "total")
print(feature.result)
print("----------------------------------------------------------")

colnames(feature.result.acc) = c("aac1", "aac2", "aac3", "aac4", "aac5", "total acc")
print(feature.result.acc)
print("----------------------------------------------------------")

colnames(feature.result.ctdc) = c("ctdc1", "ctdc2", "ctdc3", "ctdc4", "ctdc5", "total ctdc")
print(feature.result.ctdc)
print("----------------------------------------------------------")

colnames(feature.result.ctdc) = c("ctdd1", "ctdd2", "ctdd3", "ctdd4", "ctdd5", "total ctdd")
print(feature.result.ctdc)
print("----------------------------------------------------------")

colnames(feature.result.ctdt) = c("ctdt1", "ctdt2", "ctdt3", "ctdt4", "ctdt5", "total ctdt")
print(feature.result.ctdt)
print("----------------------------------------------------------")