setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(protr)
library(stringr)

main_data.files = cbind(c("chlo1.txt","chlo2.txt","chlo3.txt","chlo4.txt","chlo5.txt"))
main_data.files = rbind(main_data.files, cbind(c("cytop1.txt","cytop2.txt","cytop3.txt","cytop4.txt","cytop5.txt")))
main_data.files = rbind(main_data.files, cbind(c("cytos1.txt","cytos2.txt","cytos3.txt","cytos4.txt","cytos5.txt")))
main_data.files = rbind(main_data.files, cbind(c("er1.txt","er2.txt","er3.txt","er4.txt","er5.txt")))
main_data.files = rbind(main_data.files, cbind(c("extr1.txt","extr2.txt","extr3.txt","extr4.txt","extr5.txt")))
main_data.files = rbind(main_data.files, cbind(c("golgi1.txt","golgi2.txt","golgi3.txt","golgi4.txt","golgi5.txt")))
main_data.files = rbind(main_data.files, cbind(c("lyso1.txt","lyso2.txt","lyso3.txt","lyso4.txt","lyso5.txt")))
main_data.files = rbind(main_data.files, cbind(c("mito1.txt","mito2.txt","mito3.txt","mito4.txt","mito5.txt")))
main_data.files = rbind(main_data.files, cbind(c("nuc1.txt","nuc2.txt","nuc3.txt","nuc4.txt","nuc5.txt")))
main_data.files = rbind(main_data.files, cbind(c("pero1.txt","pero2.txt","pero3.txt","pero4.txt","pero5.txt")))
main_data.files = rbind(main_data.files, cbind(c("plas1.txt","plas2.txt","plas3.txt","plas4.txt","plas5.txt")))
main_data.files = rbind(main_data.files, cbind(c("vacu1.txt","vacu2.txt","vacu3.txt","vacu4.txt","vacu5.txt")))


dividers = c(2,3,4,5)

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
        num_feature.test = extractCTDD(seq)
        num_feature.test = extractCTDC(seq)
        num_feature.test = extractCTDT(seq)
        num_feature.test = extractAAC(seq)
        
        seq.length = nchar(seq)
        part_length = round(seq.length/divider_i)
        
        if(exists("num_feature_ctdd")){
          rm("num_feature_ctdd")
        }
        
        if(exists("num_feature_ctdc")){
          rm("num_feature_ctdc")
        }
        
        if(exists("num_feature_ctdt")){
          rm("num_feature_ctdt")
        }
        
        if(exists("num_feature_aac")){
          rm("num_feature_aac")
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
          
          if(!exists("num_feature_ctdd")){
            assign("num_feature_ctdd", cbind(extractCTDD(seq.part)))
          } else {
            num_feature_ctdd = rbind(num_feature_ctdd, cbind(extractCTDD(seq.part)))
          }
          
          if(!exists("num_feature_ctdc")){
            assign("num_feature_ctdc", cbind(extractCTDC(seq.part)))
          } else {
            num_feature_ctdc = rbind(num_feature_ctdc, cbind(extractCTDC(seq.part)))
          }
          
          if(!exists("num_feature_ctdt")){
            assign("num_feature_ctdt", cbind(extractCTDT(seq.part)))
          } else {
            num_feature_ctdt = rbind(num_feature_ctdt, cbind(extractCTDT(seq.part)))
          }
          
          if(!exists("num_feature_aac")){
            assign("num_feature_aac", cbind(extractAAC(seq.part)))
          } else {
            num_feature_aac = rbind(num_feature_aac, cbind(extractAAC(seq.part)))
          }
        }
        
        datum.num = cbind(t(num_feature_ctdd), t(num_feature_ctdc), t(num_feature_ctdt), t(num_feature_aac), class)
        
        if(!exists("main_data.num")){
          assign("main_data.num", datum.num)
        } else {
          main_data.num = rbind.data.frame(main_data.num, datum.num)
        }
      }, error = function(err){
        print(paste("error:",i,seq))
      })
    }
    
    write.csv(main_data.num, paste0("combination/ctdd-ctdc-ctdt-aac-divider-",divider_i,"-",file_i), row.names = FALSE, quote = FALSE)
    #=================================================================
    #segmentation process - end
    
  }
}