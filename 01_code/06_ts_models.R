
# Set up auto-regressive structure 
# - aggregate to week level 
# - consider state-space model 

# Aggregating to year-week:


data <- terror

residualize <- F

if(residualize){
  data$rate_guilty <- residuals(lm(rate_guilty ~ theater, data))
  data$jurors <- residuals(lm(jurors ~ theater, data))
}

ts_data <- data.frame(
  year_week = as.character(as.Date(min(data$date):max(data$date)),format = "%y%U"),
  date = as.Date(min(data$date):max(data$date))
) %>% 
  group_by(year_week) %>% 
  summarize(date = date[1]) %>% 
  ungroup() %>% 
  mutate(time = 1:n())

data <- data %>% 
  group_by(theater,year_week) %>% 
  summarize(
    rate_guilty = mean(rate_guilty),
    jurors = mean(jurors)
  ) %>% 
  group_by(year_week) %>% 
  summarize(
    rate_guilty = mean(rate_guilty),
    jurors = mean(jurors)
  ) %>% 
  right_join(ts_data,by = "year_week")

data$paris_2015 <- (data$date == as.Date("2015-11-15"))*1

data$berlin_2016 <- as.numeric(data$date == as.Date("2016-12-25"))

data$belgium_2016 <- as.numeric(data$date == as.Date("2016-03-27"))

data$paris_2017 <- as.numeric(data$date > as.Date("2017-04-20"))

AR1 <- lm(rate_guilty ~ lag(rate_guilty,1)  + paris_2015 +  berlin_2016  + belgium_2016, data = subset(data,date < as.Date("2017-02-19")))
mtable("AR1" = AR1)

summary(lm(jurors ~ lag(jurors,1)  + paris_2015 +  berlin_2016  + belgium_2016, data = data))

# Try leads (basically indicate shock in week of shock) --> all insignificant
summary(lm(rate_guilty ~ lag(rate_guilty,1)  + lead(paris_2015) +  lead(berlin_2016)  + lead(belgium_2016), data = data))
summary(lm(jurors ~ lag(jurors,1)  + lead(paris_2015) +  lead(berlin_2016)  + lead(belgium_2016), data = data))

summary(lm(rate_guilty ~ lag(rate_guilty,1)  + paris_2015 + lag(paris_2015) + berlin_2016  + lag(berlin_2016) + belgium_2016 + lag(belgium_2016), data = data))

arima101 <- arima(x = data$rate_guilty,order = c(1,0,1),xreg = with(data, cbind(paris_2015, berlin_2016, belgium_2016)))




pander(arima101)

pander(AR1,add.significance.stars = T)
pander(arima101,add.significance.stars = T)
stargazer(arima101,type = "latex")
mtable("arima" = summary(arima101))

summary(arima101)
arima101$var.coef

arima(x = data$jurors,order = c(1,0,1),xreg = with(data, cbind(paris_2015, berlin_2016, belgium_2016)))


ggplot(data,aes(x = time, y = rate_guilty,color = paris_2015)) +
  geom_line() + ylim(0:1)


terror_subset <- make_data(cutpoint = "2015-11-13", window = 60)

paris_plot <- 
  ggplot(data = terror,aes(x = date,y = prediction)) + 
  geom_point(
    data = terror_subset,
    aes(x = date,
        y = rate_guilty,
        color = theater),
    alpha = .8,
    size = 1.5,
    shape = 16
  ) +
  geom_line(data = subset(data,date <as.Date("2015-11-13")),aes(x = date, 
                                             y = rate_guilty)) + 
  geom_line(data = subset(data,date >as.Date("2015-11-13")),aes(x = date, 
                                              y = rate_guilty)) +
  geom_vline(xintercept = as.integer(as.Date("2015-11-13")),color = "red",
             linetype = 2,size = .25) +
  geom_text_repel(
    aes(x = x, y = y, label = label),
    data = data.frame(
      x = as.Date("2015-11-13"), y = .8,label = "Terror Attack on Paris"
    ),
    size = 3,
    point.padding = unit(0.8, "lines")
  ) +
  scale_y_continuous(limits = 0:1,name = "",labels = scales::percent) +
  scale_x_date(
    date_breaks = "1 month",date_labels = "%b %Y",name = "",
    limits = c(as.Date("2015-10-01"), 
               as.Date("2015-12-30")
    )
  ) +
  guides(color = FALSE,shape = FALSE) +
  theme_minimal() +
  scale_color_ptol() +
  ggtitle("Share of guilty verdicts in screenings of 'Terror'")

pdf("02_paper/images/paris_weekly_average.pdf",width = 5,height = 5)
paris_plot
dev.off()



# Aggregating to day:


data <- make_data(cutpoint = "2015-11-13", window = 60)

ts_data <- data.frame(
  year_week = as.character(as.Date(min(data$date):max(data$date)),format = "%y%U"),
  date = as.Date(min(data$date):max(data$date))
) %>% 
  mutate(time = 1:n())

data <- data %>% 
  group_by(theater,date) %>% 
  summarize(
    rate_guilty = mean(rate_guilty)
  ) %>% 
  group_by(date) %>% 
  summarize(
    rate_guilty = mean(rate_guilty)
  ) %>% 
  right_join(ts_data,by = "date")

data$paris_2015 <- data$date > as.Date("2015-11-13")


ggplot(data,aes(x = time, y = rate_guilty,color = paris_2015)) +
  geom_line() + ylim(0:1)










