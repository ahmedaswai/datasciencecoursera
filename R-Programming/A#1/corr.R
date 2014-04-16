corr <- function(directory, threshold = 0) {
  completed_cases=complete(directory)
 
  v=c()
  file_names=completed_cases[completed_cases$nobs>threshold,1]
    for (m in file_names)
    {
      file_path=paste(directory,'/',get_proper_file_name(m),'.csv',sep='')
     
      frm<-read.csv(file_path)
      
      v=append(v,cor(frm$sulfate,frm$nitrate,use="pairwise.complete.obs"))
    }
  
  v
  
  
}