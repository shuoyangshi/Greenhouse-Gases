library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(MASS)
library(caTools)
library(corrplot)
library(reshape2)
library(tseries)
library(forecast)
library(cowplot)
library(randomForest)
library(caret)
library(glmnet)
library(astsa)


### Data preparation of global temperatures ###

# Import dataset
data_temp <- read.csv("Temperature Anomalies.csv", header = TRUE, row.names = 1)
data_temp <- data_temp[,-c(13,14,15,16,17,18)]
str(data_temp)
head(data_temp)

n_row <- nrow(data_temp)
n_col <- ncol(data_temp)
row_list <- rownames(data_temp)
col_list <- colnames(data_temp)

# combine "year" and "month" to "date"
list_date <- c()
for (i in (1:n_row)) {
  for (j in (1:n_col)) {
    list_date <- c(list_date, paste(col_list[j], row_list[i], sep = "-"))
  }
}

list_temp <- c()
for (i in (1:n_row)) {
  for (j in (1:n_col)) {
    list_temp <- c(list_temp, data_temp[i,j])
  }
}

global_temp <- data.frame(list_date, as.numeric(list_temp))
colnames(global_temp) <- c("date", "avg_temp_C")

# Change "date" to date format
global_temp$date <- as.Date(paste("01-", global_temp$date, sep = ""), format = "%d-%b-%Y")

str(global_temp)

# Check missing values
unique(is.na(global_temp))
sum(is.na(global_temp$avg_temp_C))

# Plot of Global Temperatures vs Year
global_temp %>%
  ggplot(aes(x = date,
             y = avg_temp_C)) +
  geom_point(color = "brown") +
  labs(title = "Global Temperatures",
       subtitle = "Temperature Anomalies: base period 1951-1980") +
  xlab("Year") +
  ylab("Land-Ocean Temperature Anomalies (Degrees Celsius)") +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


### Data preparation of greenhouse gases ###

# Import datasets: CO2 (ppm), CH4 (ppb), N2O (ppb)
# Note: 1ppm = 1000ppb

### CO2 ###
data_co2 <- read.table("co2_mm_gl.txt", head = TRUE)
# str(data_co2)
data_co2$date <- as.Date(paste("01-", paste(data_co2$month, data_co2$year, sep = "-"), sep = ""),
                         format = "%d-%m-%Y")
df_co2 <- data.frame(data_co2$decimal, data_co2$date, data_co2$average)
colnames(df_co2) <- c("decimal", "date", "co2_avg_ppm")
str(df_co2)

### CH4 ###
data_ch4 <- read.table("ch4_mm_gl.txt", head = TRUE)
# str(data_ch4)
data_ch4$date <- as.Date(paste("01-", paste(data_ch4$month, data_ch4$year, sep = "-"), sep = ""),
                         format = "%d-%m-%Y")
df_ch4 <- data.frame(data_ch4$decimal, data_ch4$date, data_ch4$average/1000)
colnames(df_ch4) <- c("decimal", "date", "ch4_avg_ppm")
str(df_ch4)

### N2O ###
data_n2o <- read.table("n2o_mm_gl.txt", head = TRUE)
# str(data_n2o)
data_n2o$date <- as.Date(paste("01-", paste(data_n2o$month, data_n2o$year, sep = "-"), sep = ""),
                         format = "%d-%m-%Y")
df_n2o <- data.frame(data_n2o$decimal, data_n2o$date, data_n2o$average/1000)
colnames(df_n2o) <- c("decimal", "date", "n2o_avg_ppm")
str(df_n2o)

# Plot of CO2 vs Year
df_co2 %>%
  ggplot(aes(x = date,
             y = co2_avg_ppm)) +
  geom_point(size = 0.8, color = "red") +
  labs(title = "CO2 Concentrations") +
  xlab("Year") +
  ylab("CO2 in Dry Air (ppm)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Plot of CH4 vs Year
df_ch4 %>%
  ggplot(aes(x = date,
             y = ch4_avg_ppm)) +
  geom_point(size = 0.8, color = "blue") +
  labs(title = "CH4 Concentrations") +
  xlab("Year") +
  ylab("CH4 in Dry Air (ppm)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Plot of N2O vs Year
df_n2o %>%
  ggplot(aes(x = date,
             y = n2o_avg_ppm)) +
  geom_point(size = 0.8, color = "green") +
  labs(title = "N2O Concentrations") +
  xlab("Year") +
  ylab("N2O in Dry Air (ppm)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Join three datesets
df_gases <- merge(x = df_co2,
                  y = merge(x = df_n2o, y = df_ch4, by = "decimal", all.y = TRUE),
                  by = "decimal",
                  all.y = TRUE)
df_gases <- subset(df_gases, select = c(decimal, date.y, co2_avg_ppm, n2o_avg_ppm, ch4_avg_ppm))
df_gases <- 
  df_gases %>% 
  rename(date = date.y)
str(df_gases)

# Check missing values
unique(is.na(df_gases))

# Assign missing value to N2O during 1983-2001.
# Because from the plot of N2O, we could find there is a strong linear
# relationship between year and its concentration.

model_n2o <- lm(n2o_avg_ppm ~ decimal, df_n2o)
summary(model_n2o)
# Really high R^2, really good model. Can use the model to estimate past values.
missing_n2o <- filter(df_gases, is.na(n2o_avg_ppm))
pred_n2o <- predict(model_n2o, newdata = missing_n2o)
exist_n2o <- filter(df_gases, !is.na(n2o_avg_ppm))
# new n2o (assign predicted values to NA)
n2o_avg_ppm_ <- c(pred_n2o, exist_n2o$n2o_avg_ppm)

# Create a dataset of gases without NA & update gases' concentrations to deviations
# from the corresponding 1983 means
# Because our temperature data are deviations from the corresponding 1951-1980 means
# However, we don't have gases data during 1951-1980
# Maybe just use means of 1983 is a better approach
gases <- data.frame(df_gases$decimal, df_gases$date, df_gases$co2_avg_ppm,
                    n2o_avg_ppm_, df_gases$ch4_avg_ppm)
gases[,c(3,4,5)] <- scale(gases[,c(3,4,5)])
colnames(gases) <- c("decimal", "date", "scaled_co2", "scaled_n2o", "scaled_ch4")


### Data preparation: merge temperature and gases ###

global_warming <- merge(x = global_temp, y = gases, by = "date", all.y = TRUE)
str(global_warming)
head(global_warming)
# Check missing values
unique(is.na(global_warming))
summary(global_warming)


### Data visualization before building models ###

# correlation
res <- cor(global_warming[,c(4, 5, 6, 2)])
res
corrplot(res, method = "ellipse", type = "upper", title = "Correlations Between Variables",
         mar = c(0,0,1,0), tl.col = "black", tl.srt = 45, tl.cex = 1)

df_plot <- melt(global_warming[,c(1, 4, 5, 6)], id = "date")
df_plot %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Greenhouse Gases Concentrations") +
  xlab("Year") +
  ylab("Concentrations in Dry Air (ppm)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

df_plot %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Greenhouse Gases Concentrations") +
  xlab("Year") +
  ylab("log Concentrations in Dry Air (ppm)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_log10() +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

df_plot1 <- melt(global_warming[,c(2, 4, 5, 6)], id = "avg_temp_C")
df_plot1 %>%
  ggplot(aes(x = avg_temp_C, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Greenhouse Gases Concentrations vs Global Temperatures") +
  xlab("Global Temperatures Anomalies (Degrees Celsius)") +
  ylab("Concentrations in Dry Air (ppm)") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

df_plot1 %>%
  ggplot(aes(x = avg_temp_C, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Greenhouse Gases Concentrations vs Global Temperatures") +
  xlab("Global Temperatures Anomalies (Degrees Celsius)") +
  ylab("log Concentrations in Dry Air (ppm)") +
  scale_y_log10() +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


### Model Selection ###

# Model 1: linear regression

# Monte Carlo Simulation
B <- 500
sum_mse <- 0

for (k in 1:B) {
  
  # cross validation
  split_gw = sample.split(global_warming, SplitRatio = 0.75)
  training_data = subset(global_warming, split_gw == TRUE)
  test_data = subset(global_warming, split_gw == FALSE)
  
  # model
  model1 = lm(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4, training_data)
  temp_mse = mean((test_data$avg_temp_C - predict(model1, test_data))^2)
  
  sum_mse = sum_mse + temp_mse
}

test_mse_model1 = sum_mse/B
test_mse_model1

# Model 2: LASSO

# Perform 10-fold cross-validation to select lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
X <- as.matrix(global_warming[,c(4, 5, 6)])
y <- global_warming[,2]
# Setting alpha = 1 implements lasso regression
lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)
# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min
lambda_cv
# Fit final model, get its sum of squared residuals and multiple R-squared
model2 <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model2, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_lasso_cv <- cor(y, y_hat_cv)^2

test_mse_model2 = mean((y - y_hat_cv)^2)
test_mse_model2

# Model 3: Random Forest

model3_1 <- randomForest(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4,
                         data = global_warming, proximity = TRUE)

# Now check to see if the random forest is big
rf_mse1 <- data.frame(trees = c(1:length(model3_1$mse)),
                      mse = c(model3_1$mse))

rf_mse1 %>%
  ggplot(aes(x = trees, y = mse)) +
  geom_line(color = "blue") +
  labs(title = "Variation of MSE While Number of Trees Is Changing") +
  xlab("Number of Trees") +
  ylab("MSE") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Try 1000 trees

model3_2 <- randomForest(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4,
                         data = global_warming, ntree = 1000, proximity = TRUE)

## Now check to see if the random forest is big
rf_mse2 <- data.frame(trees = c(1:length(model3_2$mse)),
                      mse = c(model3_2$mse))

rf_mse2 %>%
  ggplot(aes(x = trees, y = mse)) +
  geom_line(color = "blue") +
  labs(title = "Variation of MSE While Number of Trees Is Changing") +
  xlab("Number of Trees") +
  ylab("MSE") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Try to control how many variables are considered at each step
rf_mse3 <- vector(length = 3)
for(i in 1:3) {
  temp.model <- randomForest(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4,
                             data = global_warming, ntrees = 1000,
                             mtry = i)
  rf_mse3[i] <- temp.model$mse[length(temp.model$mse)]
}

rf_mse3

# mtry = 1 is the best

## create a model for proximities using the best value for mtry

model3 <- randomForest(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4, data = global_warming, ntrees = 1000, mtry = 1)
model3

model3$importance

df_importance <- 
  data.frame(row.names(model3$importance), 
             c(model3$importance[1], model3$importance[2], model3$importance[3]))
colnames(df_importance) <- c("gas", "importance")

df_importance %>%
  ggplot(aes(x = gas, y = importance, fill = gas, group = factor(1))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(importance, 2), vjust = -0.8, hjust = 0.5, color = gas)) +
  ylim(0, max(df_importance$importance)*1.1) +
  labs(title = "Importance of 3 Greenhouse Gases (Random Forest)") +
  xlab("Greenhouse Gases") +
  ylab("Importance") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

test_mse_model3 = model3$mse[length(model3$mse)]
test_mse_model3

# Model 4: Support Vector Machine

# cross validation
split_gw = sample.split(global_warming, SplitRatio = 0.75)
training_data = subset(global_warming, split_gw == TRUE)
test_data = subset(global_warming, split_gw == FALSE)

# Train our data on different algorithms
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model4 <- train(avg_temp_C ~ scaled_co2 + scaled_n2o + scaled_ch4,
                data = training_data,
                method = "svmLinear",
                trControl = trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)

model4

test_pred <- predict(model4, newdata = test_data)
test_pred
test_mse_model4 = mean((test_data$avg_temp_C - test_pred)^2)
test_mse_model4

# Compare 4 models

c(test_mse_model1, test_mse_model2, test_mse_model3, test_mse_model4)
# model3 (Random Forest) is the best one.
# CO2 is the number one contributor to global warming.


