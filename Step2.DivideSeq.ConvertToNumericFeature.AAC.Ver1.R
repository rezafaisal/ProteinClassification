setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(protr)
library(stringr)

main_data.files = c("vacu1.txt","vacu2.txt","vacu3.txt","vacu4.txt","vacu5.txt")

dividers = c(2, 3, 4, 5)

for(file_i in main_data.files){
  main_data = read.csv(paste0("dataset/",file_i), stringsAsFactors = FALSE)
  
  #segmentation process - start
  #=================================================================
  for(divider_i in dividers){
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
        num_feature.test = extractAAC(seq)
        
        seq.length = nchar(seq)
        part_length = round(seq.length/divider_i)
        
        if(exists("num_feature")){
          rm("num_feature")
        }
        
        for(j in 1:divider_i){
          if(j > 1){
            start = ((j - 1) * part_length) + 1
            end = j * part_length
            
            if(j == divider_i){
              end = seq.length
            }
          } else {
            start = 1
            end = part_length
          }
          
          seq.part = substr(seq, start, end)
          #print(seq.part)
          #print(extractCTDD(seq.part))
          
          if(!exists("num_feature")){
            assign("num_feature", cbind(extractAAC(seq.part)))
          } else {
            num_feature = rbind(num_feature, cbind(extractAAC(seq.part)))
          }
        }
        
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
    
    write.csv(main_data.num, paste0("segmented/aac-divider-",divider_i,"-",file_i), row.names = FALSE, quote = FALSE)
    #=================================================================
    #segmentation process - end
    
  }
}