## ID5059 Project 2 ##

# Installing libraries
library(tinytex)
library(rmarkdown)
library(tidyverse)
library(tidymodels)

# Load the corrplot package
library(ggcorrplot)

## 2. Organise the data

# Reading in the data
cirr_train <- read.csv("~/St. Andrews/Modules/ID5059/Project 2/train.csv")
cirr_test <- read.csv("~/St. Andrews/Modules/ID5059/Project 2/test.csv")

# Has already been split into test and train data

# Data found at 
#  https://www.kaggle.com/datasets/joebeachcapital/cirrhosis-patient-survival-prediction

# Visualising the train set
p <- ggplot() +
  geom_bar(aes(x = factor(cirr_train$Status), fill = "Train")) +
  labs(title = "Visualising the outcome", 
       x = "Status", 
       y = "Frequency")
p

# No of observations -
obs <- nrow(cirr_train)
obs

# Total no of features -
feat <- ncol(cirr_train)
feat

# Data cleaning #
# Examining data type
cirr_train %>% sapply(class) %>% as.data.frame

# Having analysed the additional detail on the columns/features provided on the 
# Kaggle site, we amend the column types as below -

# Changing to the correct column types
category_columns <- c("Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", 
                      "Edema", "Status", "Stage")

integer_columns <- c("N_Days","Age", "Cholesterol", "Copper", "Tryglicerides", 
                     "Platelets")
cont_columns <- c("Bilirubin", "Albumin", "Alk_Phos", "SGOT", "Prothrombin")

data <- cirr_train %>%
  replace(.=="null", NA) %>%
  mutate(across(all_of(category_columns), as.factor)) %>%
  mutate(across(all_of(integer_columns), as.integer))

# Check
data %>% sapply(class) %>% as.data.frame

# Missing values
missing_data_per_row <- rowSums(is.na(data))
print(missing_data_per_row[missing_data_per_row > 1])

# No row is missing more than 1 'feature' of data
# No need to drop any rows
# We need to manually remove values to perform imputation

data %>% mutate(across(colnames(data), is.na)) %>%
  summarise(across(colnames(data), sum))
# No missing values again

## 3. Data exploration

# Unique Identifier investigation
data %>% summarise_all(n_distinct)
# From this we see that 'id' is the only unique identifier and has exactly the same 
# number of unique values as the rows in the data

# Distributions
# factor - "Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", 
#                      "Edema", "Status", "Stage"

# continuous - "Bilirubin", "Albumin", "Alk_Phos", "SGOT", "Prothrombin"

# integer - "N_Days","Age", "Cholesterol", "Copper", "Tryglicerides", "Platelets"

# Categorical features
ggplot(data) + geom_bar(aes(x = Drug)) #Balanced
ggplot(data) + geom_bar(aes(x = Sex)) #Unbalanced
ggplot(data) + geom_bar(aes(x = Ascites)) #Unbalanced
ggplot(data) + geom_bar(aes(x = Hepatomegaly)) #Balanced
ggplot(data) + geom_bar(aes(x = Spiders)) #Unbalanced
ggplot(data) + geom_bar(aes(x = Edema)) #Unbalanced
ggplot(data) + geom_bar(aes(x = Stage)) #Unbalanced

# Continuous features
ggplot(data) + geom_histogram(aes(x = Bilirubin))
# long tail on right, peak at 1

ggplot(data) + geom_histogram(aes(x = Albumin))
# gaussian, mean around 3.7, longer left tail

ggplot(data) + geom_histogram(aes(x = Alk_Phos))
# long tail on right, peak at 1000

ggplot(data) + geom_histogram(aes(x = SGOT))
# long tail in right, outliers past 400

ggplot(data) + geom_histogram(aes(x = Platelets))
# gaussian, mean around 250, longer right tail

# Assessing information from order of data
ggplot(data) + geom_point(aes(x = 1:7905, y = N_Days))
# there is no discernible info on order here

ggplot(data) + geom_point(aes(x = 1:7905, y = Age))
# there is no discernible info on order here

ggplot(data) + geom_point(aes(x = 1:7905, y = Drug))
# there is no discernible info on order here

ggplot(data) + geom_point(aes(x = 1:7905, y = Stage))
# no patients in top 3 stages of disease, but no order info

# Relationships between response and attributes
pairs(~ Drug + Sex + Ascites + Hepatomegaly + Spiders + Edema + Stage, 
      data = data,
      main = "Training set of cirr data")

pairs(~ Bilirubin + Albumin + Alk_Phos + SGOT + Prothrombin, 
      data = data,
      main = "Training set of cirr data")

pairs(~ N_Days + Age + Cholesterol + Copper + Tryglicerides + Platelets, 
      data = data,
      main = "Training set of cirr data")


# factor - "Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", 
#                      "Edema", "Status", "Stage"

# continuous - "Bilirubin", "Albumin", "Alk_Phos", "SGOT", "Prothrombin"

# integer - "N_Days","Age", "Cholesterol", "Copper", "Tryglicerides", "Platelets"

# Continuous vs Continuous
ggplot(data) + geom_point(aes(x = Bilirubin, y = Albumin))
# Check from above pairs for more like this

# Continuous vs Outcome
ggplot(data) + geom_boxplot(aes(x = Bilirubin, y = Status))
# different means for all, lots of outliers 

ggplot(data) + geom_boxplot(aes(x = Albumin, y = Status))
# different means for all, similar for C and CL

ggplot(data) + geom_boxplot(aes(x = Alk_Phos, y = Status))
# different means for all, lots of outliers 

ggplot(data) + geom_boxplot(aes(x = SGOT, y = Status))
# different means for all, lots of outliers on right

ggplot(data) + geom_boxplot(aes(x = Prothrombin, y = Status))
# different means for all, lots of outliers on right

# Factor vs Factor
# factor - "Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", 
#                      "Edema", "Status", "Stage"


table(data$Drug, data$Sex)
# Only display two decimal places
options(digits=2)
prop.table(table(data$Drug, data$Sex)) * 100
# Less data for males

prop.table(table(data$Drug, data$Ascites)) * 100
# Not balanced

prop.table(table(data$Drug, data$Hepatomegaly)) * 100
# Balanced

prop.table(table(data$Drug, data$Spiders)) * 100
# Less data for Spiders = Y

prop.table(table(data$Drug, data$Edema)) * 100
# Less data for Edema = Y and S

prop.table(table(data$Drug, data$Stage)) * 100
# Unbalanced

# Perform similar for other features to check relations

# Correlations
# Continuous data
# Subset data
selected_columns_cont <- data[, c("Bilirubin", "Albumin", "Alk_Phos", "SGOT", "Prothrombin")]

# Compute the correlation matrix for the selected columns
correlation_matrix_cont <- cor(selected_columns_cont)

# Integer data
# Subset data
selected_columns_int <- data[, c("N_Days","Age", "Cholesterol", "Copper", "Tryglicerides", 
                             "Platelets")]

# Compute the correlation matrix for the selected columns
correlation_matrix_int <- cor(selected_columns_int)

# Look at interactions

