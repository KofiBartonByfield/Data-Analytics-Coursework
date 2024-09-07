rm(list = ls())
library(tidyverse)
library(ggplot2)
library(outliers)
library(fastDummies)
library(factoextra)
library(FactoMineR)
library(survival)
library(survminer)
library(randomForest)
library(fmsb)
library(rpart)
library(rpart.plot)


# Import the data
hr <- read.csv('kaggle_hr_analytics.csv')

#summary(hr)
glimpse(hr)

# dummy columns
hr <- dummy_cols(hr, c('salary', 'sales'))

# rename columns
hr <- hr %>% 
  rename(department =  sales) %>% 
  mutate(department = fct_infreq(department))

# add an ID column
hr$ID <- as.character(1:nrow(hr)) 
glimpse(hr)





##sales salary breakdown

# select the data
hr_sales_low =  sum(hr$sales_sales[hr$salary_low == 1])
hr_sales_med =  sum(hr$sales_sales[hr$salary_medium == 1])
hr_sales_high =  sum(hr$sales_sales[hr$salary_high == 1])


# Create a vector containing the sums
sales_sums <- c(hr_sales_high, hr_sales_med, hr_sales_low)
total_sales <- sum(sales_sums)
# Calculate percentages
percentages <- round((sales_sums / total_sales) * 100, 2)




# Assigning colors
bar_colors <- c("orange", "palegreen","skyblue")

# Create bar plot with specified colors
barplot(sales_sums, 
        beside = TRUE, 
        names.arg = c( "High Salary", "Medium Salary","Low Salary"), 
        main = "Sales Salary Levels", 
        xlab = "Salary Levels", 
        ylab = "Total Sales",
        col = bar_colors,
        ylim = c(0, max(sales_sums) * 1.1))

# Add percentages on the bars
text(x = barplot(sales_sums, beside = TRUE, plot = FALSE), 
     y = sales_sums + 0.5, 
     labels = paste0(percentages, "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")







## managment salary breakdown

# select new data
hr_managment_low =  sum(hr$sales_management[hr$salary_low == 1])
hr_managment_med =  sum(hr$sales_management[hr$salary_medium == 1])
hr_managment_high =  sum(hr$sales_management[hr$salary_high == 1])


# Create a vector containing the sums
hr_sums <- c(hr_managment_high,  hr_managment_med, hr_managment_low)

# Calculate total hr
total_hr <- sum(hr_sums)

# Calculate percentages
percentages <- round((hr_sums / total_hr) * 100, 2)

# Assigning colors
bar_colors <- c("#FF5733", "#33FF57", "#3366FF")
bar_colors <- c("orange", "palegreen","skyblue")

# Create bar plot with specified colors
barplot(hr_sums, 
        beside = TRUE, 
        names.arg = c( "High Salary", "Medium Salary","Low Salary"), 
        main = "Management Salary Levels", 
        xlab = "Salary Levels", 
        ylab = "Total Sales",
        col = bar_colors,
        ylim = c(0, max(hr_sums) * 1.1))


# Add percentages on the bars
text(x = barplot(hr_sums, beside = TRUE, plot = FALSE), 
     y = hr_sums + 0.5, 
     labels = paste0(percentages, "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")






## sales salary left breakdown
hr_sales_low_stay =  sum(hr$sales_sales[hr$salary_low == 1 & hr$left == 0])
hr_sales_low_left =  sum(hr$sales_sales[hr$salary_low == 1 & hr$left == 1])

hr_sales_med_stay =  sum(hr$sales_sales[hr$salary_medium == 1 & hr$left == 0])
hr_sales_med_left =  sum(hr$sales_sales[hr$salary_medium == 1 & hr$left == 1])

hr_sales_high_stay =  sum(hr$sales_sales[hr$salary_high == 1 & hr$left == 0])
hr_sales_high_left =  sum(hr$sales_sales[hr$salary_high == 1 & hr$left == 1])


# Create a vector containing the sums
sales_sums <- c(  hr_sales_high_stay,hr_sales_high_left,
                  hr_sales_med_stay, hr_sales_med_left,
                  hr_sales_low_stay,hr_sales_low_left)


total_sales <- sum(sales_sums)



sum1 = sum(sales_sums[1] , sales_sums[2])
sum2 = sum(sales_sums[3] , sales_sums[4])
sum3 = sum(sales_sums[5] , sales_sums[6])

percentages = c(round((sales_sums[1] / sum1) * 100, 2), round((sales_sums[2] / sum1) * 100, 2),
                round((sales_sums[3] / sum2) * 100, 2), round((sales_sums[4] / sum2) * 100, 2),
                round((sales_sums[5] / sum3) * 100, 2), round((sales_sums[6] / sum3) * 100, 2))




# Assigning colors
bar_colors <- c("orange", "orange", "palegreen","palegreen", "skyblue", "skyblue")

# Create bar plot with specified colors
barplot(sales_sums, 
        beside = TRUE, 
        names.arg = c("High Salary \nStayed", "High Salary \nLeft", 
                      "Medium Salary \nStayed", "Medium Salary \nLeft",
                      "Low Salary \nStayed", "Low Salary \nLeft"), 
        
        main = "Sales Salary Levels Breakdown", 
        xlab = "Salary Levels", 
        ylab = "Total Sales",
        col = bar_colors,
        ylim = c(0, max(sales_sums) * 1.1))


# Add percentages on the bars
text(x = barplot(sales_sums, beside = TRUE, plot = FALSE), 
     y = sales_sums, 
     labels = paste0(percentages, "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")



## Management salary left breakdown

hr_sales_low_stay =  sum(hr$sales_management[hr$salary_low == 1 & hr$left == 0])
hr_sales_low_left =  sum(hr$sales_management[hr$salary_low == 1 & hr$left == 1])

hr_sales_med_stay =  sum(hr$sales_management[hr$salary_medium == 1 & hr$left == 0])
hr_sales_med_left =  sum(hr$sales_management[hr$salary_medium == 1 & hr$left == 1])

hr_sales_high_stay =  sum(hr$sales_management[hr$salary_high == 1 & hr$left == 0])
hr_sales_high_left =  sum(hr$sales_management[hr$salary_high == 1 & hr$left == 1])



# Create a vector containing the sums
sales_sums <- c(  hr_sales_high_stay,hr_sales_high_left,
                  hr_sales_med_stay, hr_sales_med_left,
                  hr_sales_low_stay,hr_sales_low_left)


total_sales <- sum(sales_sums)
# Calculate percentages
percentages <- round((sales_sums / total_sales) * 100, 2)


sum1 = sum(sales_sums[1] , sales_sums[2])
sum2 = sum(sales_sums[3] , sales_sums[4])
sum3 = sum(sales_sums[5] , sales_sums[6])

percentages = c(round((sales_sums[1] / sum1) * 100, 2), round((sales_sums[2] / sum1) * 100, 2),
                round((sales_sums[3] / sum2) * 100, 2), round((sales_sums[4] / sum2) * 100, 2),
                round((sales_sums[5] / sum3) * 100, 2), round((sales_sums[6] / sum3) * 100, 2))


# Assigning colors
bar_colors <- c("orange", "orange", "palegreen","palegreen", "skyblue", "skyblue")

# Create bar plot with specified colors
barplot(sales_sums, 
        beside = TRUE, 
        names.arg = c("High Salary \nStayed", "High Salary \nLeft", 
                      "Medium Salary \nStayed", "Medium Salary \nLeft",
                      "Low Salary \nStayed", "Low Salary \nLeft"), 
        
        main = "Management Salary Levels", 
        xlab = "Salary Levels", 
        ylab = "Total Sales",
        col = bar_colors,
        ylim = c(0, max(sales_sums) * 1.1))


# Add percentages on the bars
text(x = barplot(sales_sums, beside = TRUE, plot = FALSE), 
     y = sales_sums, 
     labels = paste0(percentages, "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")









## Random Forest
library(dplyr)
set.seed(42)

# CHANGE HERE!!!
variable = "Management"
hr_sales = hr[hr$sales_management == 1, ]




hr_sales$left <- factor(hr_sales$left)



#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(hr_sales), replace=TRUE, prob=c(0.7,0.3))
hr_train  <- hr_sales[sample, ]
hr_test   <- hr_sales[!sample, ]




set.seed(42)

# Fit the random forest model
rf <- randomForest(left ~ number_project + 
                     time_spend_company + 
                     last_evaluation + 
                     average_montly_hours + 
                     salary + 
                     Work_accident + 
                     promotion_last_5years +
                     satisfaction_level
                   , 
                   
                   data = hr_train,
                   type = "classification")



# print confusion matrix
print(rf$confusion)


## look at the rf



# Extract confusion matrix data with percentages
conf_data <- rf$confusion[, 1:3]

# Separate negative and positive predictions
conf_neg <- conf_data[, 1]
conf_pos <- conf_data[, 2]

# Set up the plotting area
par(mfrow = c(1, 2))

# Create bar plot for negative predictions
neg_bar <- barplot(conf_neg, 
                   xlab = "Negative Predictions", 
                   ylab = "Predictions", 
                   col = "#FF5733",
                   names.arg = c("Correct", "Incorrect"),
                   ylim = c(0, max(conf_neg) * 1.1))


# Add percentage labels
text(x = neg_bar[2], 
     y = conf_neg[2], 
     label = paste0(round(conf_data[1, 3] * 100), "%"), 
     pos = 3, 
     col = "black")

text(x = neg_bar[1], 
     y = conf_neg[1], 
     label = paste0(round((1 - conf_data[1, 3]) * 100), "%"), 
     pos = 3, 
     col = "black")



# Create bar plot for positive predictions
pos_bar <- barplot(conf_pos, 
                   xlab = "Positive Predictions", 
                   ylab = "Predictions", 
                   col = "#33FF57",
                   names.arg = c("Incorrect", "Correct"),
                   ylim = c(0, max(conf_pos) * 1.1))


# Add percentage labels
text(x = pos_bar[2], 
     y = conf_pos[2], 
     label = paste0(round((1 - conf_data[2, 3]) * 100), "%"), 
     pos = 3, 
     col = "black")

text(x = pos_bar[1], 
     y = conf_pos[1], 
     label = paste0(round(conf_data[2, 3] * 100), "%"), 
     pos = 3, 
     col = "black")


# Add a title
title(main = paste0("Random Forest Predictions for ", variable),
      outer = TRUE, 
      line = -1.5)







## Look at the accuracy


library(pROC)


# plot size
par(mfrow = c(1, 1))

# Predictions for training data
pred_train <- predict(rf, 
                      newdata = hr_train, 
                      type = "prob")[, 2]

# ROC curve for training data
mailing_train_roc <- roc(hr_train$left, pred_train)

# Plot ROC curve for training data
plot(mailing_train_roc, 
     main = paste0("ROC Curve for the Random Forest - Training Data for ",  variable),
     col = "darkblue", 
     lwd = 2,
     cex.lab = 1.5)

legend("bottomright", 
       legend = paste("AUC =", round(auc(mailing_train_roc), 4)),
       col = "darkblue", 
       lwd = 2, 
       cex = 1.2)





# plot size
par(mfrow = c(1, 1))

# Predictions for test data
pred_test <- predict(rf, newdata = hr_test, type = "prob")[, 2]

# ROC curve for test data
mailing_test_roc <- roc(hr_test$left, pred_test)


# Plot ROC curve for test data
plot(mailing_test_roc, 
     main = paste0("ROC Curve for the Random Forest - Test Data for ",  variable),
     col = "red", 
     lwd = 2,
     cex.lab = 1.5)

# display a legend
legend("bottomright", 
       legend = paste("AUC =", round(auc(mailing_test_roc), 4)),
       col = "red", 
       lwd = 2, 
       cex = 1.2)







## Variable Importance

# plot dimensions
par(mar = c(5, 12, 4, 2), mfrow = c(1,1))

# Get variable importance data from the random forest model
var_imp <- data.frame(Variables = row.names(rf$importance), Importance = rf$importance[,1])

# put the data in decreasing order
var_imp <- var_imp[order(var_imp$Importance, decreasing = TRUE), ]


# define the colours be used
col <- c("pink", 
         "lightblue", 
         "lightgreen", 
         "orange", 
         "plum", 
         "lightgrey")



# Create a bar plot with sorted values
barplot(var_imp$Importance, 
        names.arg = var_imp$Variables,
        xlab = "Importance",
        main = paste0("Variable Importance Plot for ", variable), 
        col = col, 
        cex.names = 1.1, 
        cex.axis = 1.2, 
        las = 1,
        horiz = TRUE)  # <- this line makes the bars horizontal








## Logistic Regression

# Fit logistic regression model
logit_model <- glm(left ~ number_project + 
                     time_spend_company + 
                     last_evaluation + 
                     average_montly_hours + 
                     salary + 
                     Work_accident + 
                     promotion_last_5years +
                     satisfaction_level,
                   
                   data = hr_sales, 
                   family = binomial)

# Summary of the model
log_sum = summary(logit_model)

# Interpret coefficients and their significance
log_sum$coefficients

  

## plot it

# Extract coefficients from the summary
coefficients <- log_sum$coefficients[2:10, 0:1]



# Plot
par(mar = c(5, 10, 4, 2), mfrow = c(1,1))  # Adjust margin to accommodate longer variable names

barplot(coefficients, 
        horiz = TRUE, 
        col = ifelse(coefficients < 0, "#33FF57", "#FF5733"),
        las = 1,
        xlab = "Significance",
        main = paste0("Variable Effect Plot for ", variable), 
)

legend("bottomleft", 
       legend = c("More Likely to Stay if increased", "More Likely to Leave if increased"), 
       fill = c("green", "red"))




## check the accuracy:


library(pROC)

# Compute predicted probabilities
predicted_probs <- predict(logit_model, type = "response")

# Compute ROC curve
roc_curve <- roc(hr_sales$left, predicted_probs)

# Plot ROC curve
plot(roc_curve, 
     col = "darkblue",
     main = paste0("ROC Curve for the Logistic Regression - for ",  variable))


legend("bottomright", 
       legend = paste("AUC =", round(auc(roc_curve), 4)),
       col = "darkblue", 
       lwd = 2, 
       cex = 1.2)



















































