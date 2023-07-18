install.packages("tidyr") 
install.packages("janitor")
install.packages("dplyr") 
install.packages("tidyverse") 

library("tidyr")
library("janitor") 
library("dplyr") 
library("tidyverse") 

database <- read.csv("data/index.csv")

# H0: the difference in unemployment rate between 2000 and 2010 was not significant 
# HA: the difference in unemployment rate between 2000 and 2010 was significant 

summary(database)

year_UR <- filter(database, !is.na(Unemployment.Rate), .by = "Year")
year_UR <- select(year_UR, Year, Unemployment.Rate)


## test 1 #### 

# find UR distribution in 2000 and 2010 
UR_1982 <- filter(year_UR, Year=="1982") 

UR_2000 <- filter(year_UR, Year=="2000") 

UR_2010 <- filter(year_UR, Year=="2010") 

# years <- c(UR_1982, UR_2000, UR_2010) 

years <- c("1982", "2000", "2010") 
UR_years <- database |> 
  filter(Year %in% years, !is.na(Unemployment.Rate)) 
# UR_years <- select(UR_years, Year, Unemployment.Rate) 

factor(UR_years$Year) 

a <- aov(Unemployment.Rate ~ Year, UR_years) 
summary(a) 
TukeyHSD(a) 

# test distributions 
t.test(UR_2000, UR_2010)



## test 2 #### 

# find average UR over all years 
chi_squared <- table(database$Inflation.Rate, database$Unemployment.Rate) 
results <- chisq.test(chi_squared) 
results$p.value
results$residuals





# find averageUR distribution in 1954 and 2017 
UR_1954 <- filter(year_UR, Year=="1954") 

UR_2017 <- filter(year_UR, Year=="2017") 




# find average UR in Y1 
# find average UR in last year 
# slope 

# test slope difference 

t.test(database, )