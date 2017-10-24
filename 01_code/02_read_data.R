
if(scrape_again){
  
  source("01_code/01_scrape_data.R")
  
  today <- Sys.Date()
  
  write.csv(x = terror,file = paste0("00_data/",today,".csv"),row.names = FALSE)
  
  rm(today)
  
} else {
  
  file_dates <- as.Date(gsub("\\.csv","",list.files(path = "00_data/")))
  
  most_recent <- sort(file_dates)[1]
  
  file_path <- paste0("00_data/",most_recent,".csv")
  
  terror <- read.csv(file = file_path,stringsAsFactors = F)
  
  terror$date <- as.Date(terror$date)
  
  rm(file_dates,most_recent,file_path)
  
}


rm(scrape_again)

# Remove observation with negative values
terror <- terror[-which(terror$not_guilty == -1),]

