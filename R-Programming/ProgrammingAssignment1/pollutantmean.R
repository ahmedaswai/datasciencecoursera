pollutantmean <- function(directory, pollutant, id = 1:332) {
  cum_mean <-0
  total_data_Frame<-data.frame()
  for(monitor in id)
  {
    file_name=paste(directory,'/',get_proper_file_name(monitor),'.csv',sep='')
    total_data_Frame<- rbind(total_data_Frame,read.csv(file_name))
  }
  
  cum_mean<-mean(na.rm=TRUE,total_data_Frame[[pollutant]])
  
}
get_proper_file_name<-function(id)
{
  st_number <-as.character(id)
  if(nchar(st_number)==1)
  {
    st_number=paste('00',st_number,sep='')
  }
  else  if (nchar(st_number)==2)
  {
    st_number=paste('0',st_number,sep='')
  }
  st_number
}