setwd("C:/Users/faisal/OneDrive - eSevens/HD Backup/My University/Study/Kanazawa University/Riset/Code/R/AminoAcidComposition")
rm(list = ls())

library(ggplot2)
library(Rtsne)

perplexity_i = 30
cross_num = 5
file_index = c(1:cross_num)
file_name_pattern = c("chlo", "cytop", "cytos", "er", "extr", "golgi", "lyso", "mito", "nuc", "pero", "plas", "vacu")
dividers = c(1,2,3,4,5)
cross_i = 1

for(divider_i in dividers){
  file_prefix = paste0("aac-divider-",divider_i,"-")
  
  #mengumpulkan data training
  #======================================================================
  for(file_i in file_index[-cross_i]){
    for(file_name_i in file_name_pattern){
      main_data.train.file = paste0("segmented/", file_prefix, file_name_i, file_i, ".txt")
      if(!exists("main_data.train")){
        assign("main_data.train", read.csv(main_data.train.file, stringsAsFactors = FALSE))
      } else {
        main_data.train = rbind.data.frame(main_data.train, read.csv(main_data.train.file, stringsAsFactors = FALSE))
      }
    }
  }
  main_data.train$class = as.factor(main_data.train$class)
  
  #mengumpulkan data testing
  #======================================================================
  for(file_name_i in file_name_pattern){
    main_data.test.file = paste0("segmented/", file_prefix, file_name_i, cross_i, ".txt")
    if(!exists("main_data.test")){
      assign("main_data.test", read.csv(main_data.test.file, stringsAsFactors = FALSE))
    } else {
      main_data.test = rbind.data.frame(main_data.test, read.csv(main_data.test.file, stringsAsFactors = FALSE))
    }
  }
  main_data.test$class = as.factor(main_data.test$class)
  
  #menggabungkan data
  main_data.all = rbind.data.frame(main_data.train, main_data.test)
  
  #tsne
  main_data.matrix = as.matrix(main_data.all[,-ncol(main_data.all)])
  main_data.tsne = Rtsne(main_data.matrix,dims=2,PCA=T,max_iter=1000, perplexity=perplexity_i, check_duplicates=FALSE)
  main_data.tsne.result = cbind.data.frame(main_data.tsne$Y, main_data.all[,ncol(main_data.all)])
  colnames(main_data.tsne.result) = paste0("V",c(1:ncol(main_data.tsne.result)))
  colnames(main_data.tsne.result)[ncol(main_data.tsne.result)] = "class_label"
  main_data.all.tsne = main_data.tsne.result
  
  plot.main_data.all.tsne <- ggplot(main_data.all.tsne, aes(x = V1, y =V2, group = factor(class_label), color=factor(class_label))) + geom_point() + ggtitle(paste("Perplexity: ", perplexity_i)) + theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank()) + coord_fixed() 
  print(plot.main_data.all.tsne)
  
  #draw plot
}