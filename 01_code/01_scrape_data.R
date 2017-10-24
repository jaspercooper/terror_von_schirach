# Read html ---------------------------------------------------------------

terr_html <- read_html("http://terror.theater/cont/results_detail/de")

terr_scrape <- 
  terr_html %>% 
  html_nodes("#accordion .small+ span , .panel-body div span") %>%
  html_text()

# Clean html --------------------------------------------------------------

# Scrubb fullstops
terr_scrape_scrubbed <- gsub("\\.","",terr_scrape)

# Find theater names
theater_names <- terr_scrape[is.na(sapply(terr_scrape_scrubbed,as.numeric))]

# Split text up by theater names 
starts <- match(x = theater_names,table = terr_scrape) + 1
ends <- c(starts-2,length(terr_scrape))[-1]

chunks <- mapply(
  start = starts,
  end = ends,
  FUN = function(start,end,data){
    data[start:end]
  },
  MoreArgs = list(data = terr_scrape))

names(chunks) <- theater_names

# Build datasets ----------------------------------------------------------

make_dataset <- function(chunk,theater_name){
  data <- matrix(chunk,ncol = 3,byrow = T)
  data <- as.data.frame(data)
  names(data) <- c("date","not_guilty","guilty")
  data$date <- gsub("\\.","-",data$date)
  data$date <- as.Date(data$date,format = "%d-%m-%Y")
  data$not_guilty <- as.integer(as.character(data$not_guilty))
  data$guilty <- as.integer(as.character(data$guilty))
  data <- cbind.data.frame(theater = rep(theater_name,nrow(data)),data)
  return(data)
}

data_chunks <- lapply(
  X = 1:length(chunks),
  FUN = function(i){
    make_dataset(chunk = chunks[[i]],
              theater_name = theater_names[i]
    )
  })

terror <- do.call(what = rbind,args = data_chunks)

rm(chunks,data_chunks,ends,starts,terr_html,terr_scrape,
   terr_scrape_scrubbed,theater_names,make_dataset)













