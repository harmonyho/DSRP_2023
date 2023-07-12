database <- read.csv("data/index.csv")

install.packages("dplyr") 
library("ggplot2")
library("dplyr")

summary(database)

unemployment <- filter(database, Unemployment.Rate >= 5)

smallset_targets <- select(database, c(Federal.Funds.Upper.Target:Federal.Funds.Lower.Target, Year:Month)) 

mutate(smallset_targets, funds_range = Federal.Funds.Upper.Target - Federal.Funds.Lower.Target)
mutate(database, mean_lower_funds = mean(Federal.Funds.Lower.Target, na.rm = TRUE))

summarize(database, mean_upper_funds = mean(Federal.Funds.Upper.Target, na.rm = TRUE), count = n(), .by = Year)

arrange(database, Month)

ggplot(database, aes(x = Year, y = Federal.Funds.Upper.Target), na.rm = TRUE) + 
  geom_jitter() + 
  geom_smooth() + 
  labs(title = "Federal Funds Upper Target by Year")

