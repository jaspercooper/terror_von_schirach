
# Main plots for post -----------------------------------------------------


# Paris Plot --------------------------------------------------------------
data <- make_data(cutpoint = "2015-11-13",window = 60)

predict_model <- paris_linear_6

paris_pre <- expand.grid(
  jurors = round(mean(data$jurors),0),
  time = min(data$time[data$paris_2015==0]):max(data$time[data$paris_2015==0]),
  paris_2015 = 0,
  theater = unique(data$theater)
)

# If poisson:
# paris_pre$prediction <- predict(object = predict_model,newdata = paris_pre,type = "response") / round(mean(data$jurors),0)
# If linear:
paris_pre$prediction <- predict(object = predict_model,newdata = paris_pre,type = "response")



paris_pre_predictions <- 
  paris_pre %>% 
  group_by(time) %>% 
  summarize(
    prediction = mean(prediction)
  )

paris_post <- expand.grid(
  jurors = round(mean(data$jurors),0),
  time = min(data$time[data$paris_2015==1]):max(data$time[data$paris_2015==1]),
  paris_2015 = 1,
  theater = unique(data$theater)
)

# If poisson:
# paris_post$prediction <- predict(object = predict_model,newdata = paris_post,type = "response") / round(mean(data$jurors),0)
# If linear:
paris_post$prediction <- predict(object = predict_model,newdata = paris_post,type = "response")

paris_post_predictions <- 
  paris_post %>% 
  group_by(time) %>% 
  summarize(
    prediction = mean(prediction)
  )

predictions <- rbind(paris_pre_predictions,paris_post_predictions)

times <- with(
  data,
  data.frame(
    date = as.Date(unique(date[time == min(time)]):unique(date[time == max(time)])),
    time = min(time):max(time)
  ))

predictions <- merge(predictions,times,by = "time")
paris_pre_predictions <- merge(paris_pre_predictions,times,by = "time")
paris_post_predictions <- merge(paris_post_predictions,times,by = "time")

paris_plot <- 
  ggplot(data = predictions,aes(x = date,y = prediction)) + 
  geom_point(
    data = data,
    aes(x = date,
        y = rate_guilty,
        color = theater),
    alpha = .8,
    size = 1.5,
    shape = 16
    ) +
  geom_line(data = paris_pre_predictions,aes(x = date, 
                                             y = prediction)) + 
  geom_line(data = paris_post_predictions,aes(x = date, 
                                              y = prediction)) +
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
               as.Date("2016-01-01")
               )
      ) +
  guides(color = FALSE,shape = FALSE) +
  theme_minimal() +
  scale_color_ptol() +
  ggtitle("Share of guilty verdicts in screenings of 'Terror'")


  
pdf("02_paper/images/paris_polynomial_4_linear.pdf",width = 5,height = 5)
paris_plot
dev.off()


# Brussels ----------------------------------------------------------------

data <- make_data(cutpoint = "2016-03-22", window = 60)

predict_model <- belgium_linear_6

belgium_pre <- expand.grid(
  jurors = round(mean(data$jurors),0),
  time = min(data$time[data$belgium_2016==0]):max(data$time[data$belgium_2016==0]),
  belgium_2016 = 0,
  theater = unique(data$theater)
)

# If poisson:
# belgium_pre$prediction <- predict(object = predict_model,newdata = belgium_pre,type = "response") / round(mean(data$jurors),0)
# If linear:
belgium_pre$prediction <- predict(object = predict_model,newdata = belgium_pre,type = "response")



belgium_pre_predictions <- 
  belgium_pre %>% 
  group_by(time) %>% 
  summarize(
    prediction = mean(prediction)
  )

belgium_post <- expand.grid(
  jurors = round(mean(data$jurors),0),
  time = min(data$time[data$belgium_2016==1]):max(data$time[data$belgium_2016==1]),
  belgium_2016 = 1,
  theater = unique(data$theater)
)

# If poisson:
# belgium_post$prediction <- predict(object = predict_model,newdata = belgium_post,type = "response") / round(mean(data$jurors),0)
# If linear:
belgium_post$prediction <- predict(object = predict_model,newdata = belgium_post,type = "response")

belgium_post_predictions <- 
  belgium_post %>% 
  group_by(time) %>% 
  summarize(
    prediction = mean(prediction)
  )

predictions <- rbind(belgium_pre_predictions,belgium_post_predictions)

times <- with(
  data,
  data.frame(
    date = as.Date(unique(date[time == min(time)]):unique(date[time == max(time)])),
    time = min(time):max(time)
  ))

predictions <- merge(predictions,times,by = "time")
belgium_pre_predictions <- merge(belgium_pre_predictions,times,by = "time")
belgium_post_predictions <- merge(belgium_post_predictions,times,by = "time")

belgium_plot <- 
  ggplot(data = predictions,aes(x = date,y = prediction)) + 
  geom_point(
    data = data,
    aes(x = date,
        y = rate_guilty,
        color = theater),
    alpha = .8,
    size = 1.5,
    shape = 16
  ) +
  geom_line(data = belgium_pre_predictions,aes(x = date, 
                                             y = prediction)) + 
  geom_line(data = belgium_post_predictions,aes(x = date, 
                                              y = prediction)) +
  scale_y_continuous(limits = 0:1,name = "% Guilty Verdicts") +
  geom_vline(xintercept = as.integer(as.Date("2016-03-22")),color = "red",
             linetype = 2,size = .25) +
  scale_x_date(
    date_breaks = "1 month",date_labels = "%b %Y",name = "",
    limits = c(as.Date("2016-02-01"),
               as.Date("2016-05-01")
    )
  ) +
  guides(color = FALSE,shape = FALSE) +
  theme_minimal() 


pdf("02_paper/images/belgium_polynomial_4_linear.pdf",width = 5,height = 5)
belgium_plot
dev.off()


# Descriptive plots -------------------------------------------------------

terror %>% 
  # subset(country == "Germany") %>% 
  ggplot(aes(x = date,y = rate_guilty)) +
  geom_point(aes(color = theater))+
  ylim(0:1) +
  geom_vline(xintercept = as.integer(as.Date("2016-12-19")),color = "red") +
  geom_vline(xintercept = as.integer(as.Date("2016-07-14")),color = "red") +
  geom_vline(xintercept = as.integer(as.Date("2016-03-22")),color = "red") +
  geom_vline(xintercept = as.integer(as.Date("2015-11-14")),color = "red") +
  geom_vline(xintercept = as.integer(as.Date("2016-10-17")),color = "blue") +
  scale_x_date(date_breaks = "1 month") +
  geom_smooth()




make_data(cutpoint = "2015-11-14",window = 60) %>% 
  # subset(country == "Germany") %>% 
  ggplot(aes(x = date,y = rate_guilty)) +
  geom_point(aes(color = theater))+
  ylim(0:1) +
  geom_vline(xintercept = as.integer(as.Date("2015-11-14")),color = "red") +
  scale_x_date(date_breaks = "1 month") +
  geom_smooth()






