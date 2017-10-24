setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

if(exists("prot_seq")){
  rm("prot_seq")
} 

if(exists("data_seq")){
  rm("data_seq")
} 

filename = "chlo2.b"
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
        assign("data_seq", prot_seq)
      } else {
        data_seq = rbind(data_seq, prot_seq)
      }
      rm("prot_seq")
    }
    print(line)
    if(!exists("data_seq")){
      assign("data_seq", line)
    } else {
      data_seq = rbind(data_seq, line)
    }
  } else {
    if(!exists("prot_seq")){
      assign("prot_seq", line)
    } else {
      prot_seq = paste0(prot_seq, line)
    }
  }
}

write.table(data_seq, paste0("dataset/",filename,".txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
