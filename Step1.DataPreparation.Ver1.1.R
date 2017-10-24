setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

if(exists("prot_seq")){
  rm("prot_seq")
} 

if(exists("data_seq")){
  rm("data_seq")
} 

filename = "vacu5"
class_name = "vacuolar"
con = file(paste0("dataset/", filename,".dat"), "r")
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
        assign("data_seq", cbind(prot_seq, class_name))
      } else {
        data_seq = rbind(data_seq, cbind(prot_seq, class_name))
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

colnames(data_seq) = c("seq", "class")
write.csv(data_seq, paste0("dataset/",filename,".txt"), row.names = FALSE, quote = FALSE)
