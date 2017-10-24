setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(protr)
library(stringr)

# main_data.files = cbind(c("chlo1.txt","chlo2.txt","chlo3.txt","chlo4.txt","chlo5.txt"))
# main_data.files = rbind(main_data.files, cbind(c("cytop1.txt","cytop2.txt","cytop3.txt","cytop4.txt","cytop5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("cytos1.txt","cytos2.txt","cytos3.txt","cytos4.txt","cytos5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("er1.txt","er2.txt","er3.txt","er4.txt","er5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("extr1.txt","extr2.txt","extr3.txt","extr4.txt","extr5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("golgi1.txt","golgi2.txt","golgi3.txt","golgi4.txt","golgi5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("lyso1.txt","lyso2.txt","lyso3.txt","lyso4.txt","lyso5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("mito1.txt","mito2.txt","mito3.txt","mito4.txt","mito5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("nuc1.txt","nuc2.txt","nuc3.txt","nuc4.txt","nuc5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("pero1.txt","pero2.txt","pero3.txt","pero4.txt","pero5.txt")))
# main_data.files = rbind(main_data.files, cbind(c("plas1.txt","plas2.txt","plas3.txt","plas4.txt","plas5.txt")))

main_data.files = cbind(c("nuc1.txt","nuc2.txt","nuc3.txt","nuc4.txt","nuc5.txt"))
main_data.files = rbind(main_data.files, cbind(c("plas1.txt","plas2.txt","plas3.txt","plas4.txt","plas5.txt")))

for(file_i in main_data.files){
  main_data = read.csv(paste0("dataset/",file_i), stringsAsFactors = FALSE)
  
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
      num_feature = extractCTDC(seq)
      
      datum.num = cbind(t(num_feature), class)
      
      if(!exists("main_data.num")){
        assign("main_data.num", datum.num)
      } else {
        main_data.num = rbind.data.frame(main_data.num, datum.num)
      }
    }, error = function(err){
      print(paste("error:",i,seq))
    })
  }
  
  write.csv(main_data.num, paste0("segmented/ctdc-divider-1-",file_i), row.names = FALSE, quote = FALSE)
  #=================================================================
  #segmentation process - end
}