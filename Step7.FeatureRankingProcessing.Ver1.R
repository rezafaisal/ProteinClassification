setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

feature_rangking.filename = "kknn-features-rangking-3grams-overlapped.50inc.accuracy.csv"

main_data = read.csv(paste0("3grams/result/", feature_rangking.filename), stringsAsFactors = FALSE)

key_aac = c("aac2", "aac3", "aac4", "aac5", "A", "R", "N", "D", "C", "E", "Q", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
key_ctdc = c("ctdc2", "ctdc3", "ctdc4", "ctdc5", "hydrophobicity", "normwaalsvolume", "polarity", "polarizability", "charge", "secondarystruct", "solventaccess")
key_ctdd = c("ctdd2", "ctdd3", "ctdd4", "ctdd5", "G1", "G2", "G3")
key_ctdt = c("ctdt2", "ctdt3", "ctdt4", "ctdt5", "Tr1221", "Tr1331", "Tr2332")
key_w2v = c("Word2Vec")

cross_num = 5
feature_max = 1350

if(exists("feature.result")){
  rm("feature.result")
}

for(cross_i in 1:cross_num){
  main_data.feature = main_data[cross_i, c(1:feature_max)]
  
  count_aac = 0
  count_ctdc = 0
  count_ctdd = 0
  count_ctdt = 0
  count_w2v = 0
  
  for(feature_i in main_data.feature){
    is_show = FALSE
    
    feature_type = strsplit(feature_i, "[.]")[[1]][1]
    feature_type.2 = strsplit(feature_i, "[.]")[[1]][2]
    
    if(is.element(feature_type, key_aac)){
      count_aac = count_aac + 1
      is_show = TRUE
    } 
    else if(is.element(feature_type, key_ctdc)){
      count_ctdc = count_ctdc + 1
      is_show = TRUE
    } 
    else if(is.element(feature_type, key_ctdd)){
      count_ctdd = count_ctdd + 1
      is_show = TRUE
    }
    else if(is.element(feature_type, key_ctdt)){
      count_ctdt = count_ctdt + 1
      is_show = TRUE
    }
    else if (is.element(feature_type, key_w2v)){
      count_w2v = count_w2v + 1
      is_show = TRUE
    } 
    
    if (is.element(feature_type.2, key_ctdd)){
      count_ctdd = count_ctdd + 1
      is_show = TRUE
    } else if(is.element(feature_type.2, key_ctdt)){
      count_ctdt = count_ctdt + 1
      is_show = TRUE
    }
    
    if(!is_show){
      print(feature_i)
    }
  }
  
  if(!exists("feature.result")){
    assign("feature.result", cbind(count_aac, count_ctdc, count_ctdd, count_ctdt, count_w2v, (count_aac + count_ctdc + count_ctdd + count_ctdt + count_w2v)))
  } else {
    feature.result = rbind(feature.result, cbind(count_aac, count_ctdc, count_ctdd, count_ctdt, count_w2v, (count_aac + count_ctdc + count_ctdd + count_ctdt + count_w2v)))
  }
}

colnames(feature.result) = c("aac", "ctdc", "ctdd", "ctdt", "word2vec", "total")
print(feature.result)
