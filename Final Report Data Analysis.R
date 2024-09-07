library(tidyverse) # data manipulation
library(fastDummies)



# Import the data
hr <- read.csv('kaggle_hr_analytics.csv')
# add an ID column
hr$id <- as.character(1:nrow(hr))
glimpse(hr)



# dummy columns
hr <- dummy_cols(hr, select_columns = c('salary', 'sales'))


hr %>% group_by_all() %>% 
summarize(n = n()) %>% 
filter(n > 3)  
hr <- unique(hr) # remove duplicates






library(factoextra)
library(FactoMineR)

hr_pca <- hr %>% 
  select_if(is.numeric) %>% 
  PCA

hr_num <- hr %>% 
  select(satisfaction_level, last_evaluation, number_project, average_montly_hours, time_spend_company, Work_accident, promotion_last_5years)

hr_pca <- PCA(hr_num)
fviz_pca_biplot(hr_pca, geom = 'point', col.ind = hr$left)



