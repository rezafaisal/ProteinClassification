setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(protr)
library(stringr)

main_data.files = "enzyme-all.csv"


for(file_i in main_data.files){
  main_data = read.csv(paste0("dataset/enzyme/",file_i), stringsAsFactors = FALSE)
  
  #segmentation process - start
  #=================================================================
  if(exists("main_data.num")){
    rm("main_data.num")
  }
  
  for(i in 1:nrow(main_data)){
    datum = main_data[i,]
    
    tryCatch({
      class = datum$class
      seq = datum$seq
      
      # validasi sequence
      # jika proses ini gagal maka baris yang di bawah di dalam blok ini tidak akan dieksekusi
      # dilanjutkan dengan sample berikutnya
      num_feature1 = extractCTDD(seq)
      num_feature2 = extractCTDC(seq)
      num_feature3 = extractCTDT(seq)
      num_feature4 = extractAAC(seq)
      
      datum.num = cbind(t(num_feature1), t(num_feature2), t(num_feature3), t(num_feature4), class)
      
      if(!exists("main_data.num")){
        assign("main_data.num", datum.num)
      } else {
        main_data.num = rbind.data.frame(main_data.num, datum.num)
      }
    }, error = function(err){
      print(paste("error:",i,seq))
    })
  }
  
  write.csv(main_data.num, paste0("enzymes/ctdd-ctdc-ctdt-aac-divider-1-",file_i), row.names = FALSE, quote = FALSE)
  #=================================================================
  #segmentation process - end
}