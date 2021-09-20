census80 <- filter(df, CENSUS == 80)[, c("YOB", "QOB", "LWKLYWGE")] %>% 
  mutate(YearQtr = YOB + 0.25 * QOB) %>% 
  group_by(YearQtr)  %>% 
  mutate(mean.wage = mean(LWKLYWGE))

time <- seq(30.25, 50, 0.25) # x-axis data
n <- length(time) # total number of time periods

ave.wage <- vector("numeric", length = n) # initialize y-axis data
group <- vector("numeric", length = n) # initialize group to capture Q1 across years
for (t in c(1:n)){
  loc <- match(time[t], census80$YearQtr) # find the location of the first match of time
  ave.wage[t] <- census80$mean.wage[loc] # obtain the mean wage accordingly 
  group[t] <- (t-1) %% 4 + 1
}

wage.time <- as.data.frame(cbind(time, ave.wage, group))
wage.time$group <- as.factor(wage.time$group)

ggplot(wage.time, aes(x = time, y = ave.wage)) + 
  geom_line() +
  geom_point(aes(shape = group, fill = group),size = 3) + 
  scale_shape_manual(values = c(22, 0,0,0)) + 
  geom_text(aes(label=group), size=3, nudge_y = 0.01, check_overlap = T) +
  theme(legend.position = "none") +
  xlim(30,50) +
  xlab("Year of Birth") + 
  ylab("Log Weekly Earnings") +
  ggsave("./Analysis/Output/FigureV.png", width = 8, height = 6) 
  

