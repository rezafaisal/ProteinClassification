setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

if(exists("prot_seq")){
  rm("prot_seq")
} 

if(exists("data_seq")){
  rm("data_seq")
} 

filename = "nonenzyme-487"
class_name = "notenzyme"
con = file(paste0("dataset/enzyme/", filename,".txt"), "r")
while ( TRUE ) {
  line = readLines(con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  line_first_char = substr(line, 1, 1)
  if(line_first_char == ">"){
    if(exists("prot_seq")){
      print(prot_seq)
      if(!exists("data_seq")){
        assign("data_seq", cbind(prot_seq, nchar(prot_seq), class_name))
      } else {
        data_seq = rbind(data_seq, cbind(prot_seq, nchar(prot_seq), class_name))
      }
      rm("prot_seq")
    }
    print(line)
    if(!exists("data_seq")){
      #assign("data_seq", line)
    } else {
      #data_seq = rbind(data_seq, line)
    }
  } else {
    if(!exists("prot_seq")){
      assign("prot_seq", line)
    } else {
      prot_seq = paste0(prot_seq, line)
    }
  }
}

colnames(data_seq) = c("seq", "seq_len", "class")
write.csv(data_seq, paste0("dataset/enzyme/",filename,".csv"), row.names = FALSE, quote = FALSE)
