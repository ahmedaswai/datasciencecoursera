complete <- function(directory, id = 1:332) {
  frm<-data.frame(row.names=c("id","nobs"))
  
  for(monitor in id)
  {
    file_name=paste(directory,'/',get_proper_file_name(monitor),'.csv',sep='')
    un_completed_case=0
    read_frm=read.csv(file_name)
    un_completed_case =dim(read_frm[,is.na(read_frm$sulfate)||is.na(read_frm$nitrate)] )[1]
    frm<- rbind(frm,c(monitor,un_completed_case))
  }
  frm
  
}