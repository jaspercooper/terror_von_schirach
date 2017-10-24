
# Time elapsed since start of series
terror$time <- terror$date - min(terror$date) + 1

# Month
terror$month <- as.character(terror$date,format = "%m")
# Year
terror$year <- as.character(terror$date,format = "%Y")
# Year-weeks
terror$year_week <- as.character(terror$date,format = "%y%U")

# Pre or post Berlin 2016 attack
terror$berlin_2016 <- as.numeric(terror$date > as.Date("2016-12-19"))

# Pre or post Paris 2015 attack
terror$paris_2015 <- as.numeric(terror$date > as.Date("2015-11-13"))

# Pre or post Belgium 2016 attack
terror$belgium_2016 <- as.numeric(terror$date > as.Date("2016-03-22"))

# Pre or post Paris 2017 attack
terror$paris_2017 <- as.numeric(terror$date > as.Date("2017-04-20"))

# Total number of jurors
terror$jurors <- terror$not_guilty + terror$guilty

# Rate not_guilty
terror$rate_not_guilty <- terror$not_guilty / terror$jurors

# Rate guilty
terror$rate_guilty <- terror$guilty / terror$jurors

# Country
terror$country <- "Germany"
country_matches <- c(
  "Trasnocho",
  "Jozsef",
  "effinger|sursee|zofingen",
  "@|götzis",
  "Hyogo|Nikkei"
)
countries <- c(
  "Venezuela",
  "Hungary",
  "Switzerland",
  "Austria",
  "Tokyo"
)

for (i in 1:length(countries)){
  terror$country[grepl(pattern = country_matches[i],x = terror$theater,ignore.case = TRUE)] <- countries[i]
}















