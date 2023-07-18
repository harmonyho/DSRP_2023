# install packages 
library("dplyr") 
library("parsnip") 
library("reshape2") 
library("ggplot2") 
library("rsample") 
library("MLmetrics") 
library("xgboost") 

# step 1 
database <- read.csv("data/index.csv") 
head(database) 

# step 2 
UR <- filter(database, !is.na(Unemployment.Rate), .by = "Year")
UR <- select(UR, Year, Unemployment.Rate)

# step 3 
UR_correlation <- cor(UR) |> 
  melt() |> 
  as.data.frame() 

ggplot(UR_correlation, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low="darkblue", mid="white", high="darkred", midpoint=0) + 
  theme_minimal() 

pca <- prcomp(UR, scale. = T) 
eigenvalues <- (pca$rotation)^2 
max(eigenvalues) 
summary(pca) 

# step 5 
set.seed(123) 

data_reg_split <- initial_split(UR, prop = 0.75) 

train_reg_data <- training(data_reg_split) 
test_reg_data <- testing(data_reg_split) 

# step 7 
linreg_fit <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(Unemployment.Rate ~ ., data = train_reg_data) 

summary(linreg_fit$fit) 

# step 8 
UR_pred <- test_reg_data 
UR_pred$linReg <- predict(linreg_fit, test_reg_data)$.pred 

yardstick::mae(UR_pred, truth = Unemployment.Rate, estimate = linReg) 
yardstick::rmse(UR_pred, truth = Unemployment.Rate, estimate = linReg) 

# step 7 
boost_tree_fit <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression") |> 
  fit(Unemployment.Rate ~ ., data = train_reg_data) 

boost_tree_fit$fit 

# step 8 
UR_pred <- test_reg_data 
UR_pred$logReg <- predict(boost_tree_fit, test_reg_data)$.pred 

yardstick::mae(UR_pred, truth = Unemployment.Rate, estimate = logReg) 
yardstick::rmse(UR_pred, truth = Unemployment.Rate, estimate = logReg) 
