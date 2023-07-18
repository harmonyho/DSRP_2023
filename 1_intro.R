finance_db <- read.csv("data/index.csv") 
# glimpse(finance_db)
summary(finance_db)

install.packages("ggplot2") 
library("ggplot2") 

install.packages("usethis") 
library("usethis") 
install.packages("credentials") 
library("credentials")

usethis::use_git_config(user.name = "Harmony Ho", 
                        user.email = "hamsterrnr@gmail.com") 
credentials::set_github_pat("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")

unemployment_rate <- finance_db$Unemployment.Rate
inflation_rate <- finance_db$Inflation.Rate 

mean_unemployment_rate <- mean(unemployment_rate, na.rm = TRUE) 
median_unemployment_rate <- median(unemployment_rate, na.rm = TRUE) 
range <- range(unemployment_rate, na.rm = TRUE) 
range_unemployment_rate = range[2] - range[1] 
variance_unemployment_rate <- var(unemployment_rate, na.rm = TRUE) 
stdev_unemployment_rate <- sd(unemployment_rate, na.rm = TRUE) 
iqr_unemployment_rate <- IQR(unemployment_rate, na.rm = TRUE) 

q1_unemployment_rate <- quantile(unemployment_rate, 0.25, na.rm = TRUE) 
q3_unemployment_rate <- quantile(unemployment_rate, 0.75, na.rm = TRUE) 
upper_limit <- q3_unemployment_rate + 1.5 * iqr_unemployment_rate 
lower_limit <- q1_unemployment_rate - 1.5 * iqr_unemployment_rate 

outliers = unemployment_rate[unemployment_rate < lower_limit | unemployment_rate > upper_limit]

unemployment_rate_2 <- unemployment_rate[unemployment_rate >= lower_limit & unemployment_rate <= upper_limit] 
mean_unemployment_rate_2 <- mean(unemployment_rate_2, na.rm = TRUE) 
median_unemployment_rate_2 <- median(unemployment_rate_2, na.rm = TRUE) 

ggplot(finance_db, aes(x = Year, y = unemployment_rate)) + 
  geom_point() + 
  labs(title = "Unemployment rate by year") 

ggplot(finance_db, aes(x = Year, y = unemployment_rate)) + 
  geom_jitter(aes(color = Month)) + 
  geom_smooth() + 
  labs(title = "Unemployment rate by year")  

ggplot(finance_db, aes(x = inflation_rate, y = unemployment_rate)) + 
  geom_smooth() + 
  labs(title = "Inflation rate correlation with unemployment")


