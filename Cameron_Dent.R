# Week 5
# 1. Import tidyverse library
library(tidyverse)

## set working directory to be the directing inside which is the cleaned data.
setwd('/Users/camerondent/Desktop/Data Analytics/LSE/Module 3/Assignment')

## read cleaned_reviews.csv and set to df
df <- read.csv('cleaned_reviews.csv', header=TRUE)

# I chose 'cleaned_reviews.csv' file to save time. 'language' and 'platform'
# columns have already been removed and relevant columns renamed.

# Explore the data

head(df,n=5)
summary(df)

# Exploratory Analysis
## Comment on distributions, patterns, and outliers based on visual exploration 
## of data. The relationships between loyalty_points and remuneration, age, and
## spending_score are of main interest.
### Where do you think you should investigate further?

# Create scatterplots of loyalty_points against the variables of interest.

# 1. remuneration
ggplot(data = df,
       mapping = aes(x=remuneration,y=loyalty_points)) +
  geom_point(color='red',alpha=0.5,size=1.5) +
  geom_smooth(method=lm)

# Observations: loyalty_points and remuneration have a clear positive relationship.
# The are most related when remuneration is under 58, after which point the 
# data distribution appears more random and data points deviate further from 
# the trend line. 

# 2. age
ggplot(data = df,
       mapping = aes(x=age,y=loyalty_points)) +
  geom_point(color='red',alpha=0.5,size=1.5) +
  geom_smooth(method=lm)

# Observations: There is a weak negative relation between age and loyalty_points.
# Most customers have fewer than 2000 loyalty points and most customers who have
# more than 3000 loyalty points tend to be between 30 and 40 years old. 

# 3. spending_score
ggplot(data = df,
       mapping = aes(x=spending_score,y=loyalty_points)) +
  geom_point(color='red',alpha=0.5,size=1.5) +
  geom_smooth(method=lm)

# Observations: Strong positive relationship between loyalty_points and spending_score.
# Similar to remuneration results, data points are more clustered where spending_score
# is less than 60. From 60 onwards, data points are more sparsely distributed, with
# extreme values (outliers) becoming increasingly likely and the predicting power
# of the regression diminishing.

# Create a boxplot of age groups

ggplot(data=df,
       mapping=aes(y=age,x=gender)) +
  geom_boxplot(staplewidth = 0.5,
               varwidth = TRUE,
               outlier.colour = 'red',
               fill='blue',
              alpha=0.7)

# Observations: Comparing male and female customers, I see that the mean age in both
# groups is roughly the same, slightly higher in the male group and both are 
# positively skewed because the right tails (top) are longer than the left tails. 
# The interquartile range and overall spread is greater for the male group than 
# the female group. The male group has no outliers; the female group has two 
# outliers, beyond the upper limit. 

# Create a bar chart of education

ggplot(data=df,
       mapping=aes(x=education)) +
  geom_bar(fill = 'blue')

# Observations: The vast majority of customers are university graduates.

# Create a histogram of age loyalty_points

ggplot(data=df,
       mapping = aes(x=loyalty_points)) +
  geom_histogram(fill = 'blue',
               colour = 'red',
               bins = 10
  ) 

# Observations: Most customers have under 2000 loyalty_points. From 2000 points 
# onwards, the number of customers within each bin remains below 130, steadily
# falling after 4000 loyalty points. 

# Create a boxplot of loyalty points

ggplot(data=df,
       mapping = aes(x=loyalty_points)) +
  geom_boxplot(fill = 'blue',
                 outlier.colour = 'red',
               staplewidth = 0.5
                ) 

# Observations: Mean loyalty_points is roughly 1300. There are many outliers beyond
# the upper limit. 

# Create a boxplot of loyalty points by gender
ggplot(data=df,
       mapping = aes(x=loyalty_points,y=gender)) +
  geom_boxplot(fill = 'blue',
               outlier.colour = 'red',
               staplewidth = 0.5
  ) 

# Observations: Male and female groups are very similar. Upper and lower limits
# and mean are roughly the same, with females having a slightly greater mean
# number of loyalty points. Both groups have many extreme values. 

# Plot spending_scores and loyalty_points via a scatter plot, split by gender

ggplot(data=df,
       mapping=aes(x=spending_score,y=loyalty_points)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~gender)

# Observations: Loyalty_points vs spending_score scatter plots for males and females
# yield similar results. Both groups reveal a general positive relationship between
# loyalty points and spending scores, with most data point clustering between
# spending scores 38 and 52. However, extreme values in the female plot
# demonstrate the problem they can produce when plotting a non-linear 
# trend line. Except when spending_score exceeds 83, female and male trend 
# lines are very similar.

ggplot(data=df,
       mapping=aes(x=spending_score,y=loyalty_points)) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap(~gender)

# Observations: Using the same plot but with a linear regression line, we see that
# the female and male groups produce almost identical results.

# Activity 6: Making recommendations to the business
# 1. Determine and justify the features to be used in a multiple linear regression
# State potential concerns and corrective actions.
# 2. Create a mlr and evaluate goodness of fit, interpret stats, visualise, 
# comment on usefulness, potential improvements/alternatives
# Provide a demonstration using randomly selected values.

# 1. The features

# Previous analysis using visualisations demonstrated that both genders behave
# very similarly. Therefore, there would be little gained from including the 
# variable as a feature for the multiple linear model (MLR). 
# The product variable is not well explored because there are 200 and the analysis
# could prove very granular at this stage of analysis. However, it would be worth 
# investigating further which product sell best with customers and which underperform.
# spending_score, and remuneration have proven to be important predictors 
# of loyalty_points. Age seems less important as the relationship is weaker, however
# there still appear to be enough of a relationship to justify its inclusion. 
# Therefore, the three features selected are 'age', 'remuneration', and 
# 'spending_score'.

# 2. The Model

model <- lm(loyalty_points ~ age + remuneration + spending_score, df)
model

# Coefficients:
# (Intercept)        age      remuneration  spending_score  
# -2203.06           11.06           34.01           34.18

# All three features have positive relationships with the dependent variable, 
# loyalty_points. For example, a one-unit increase in age estimates a 11.06 unit
# increase in loyalty_points. Variables remuneration and spending_score have 
# roughly the same coefficients of 34, making them more important factors than 
# age. Interestingly, when age was the sole variable in a regression, we noted
# a slight negative relationship with loyalty_points. Only after including 
# remuneration and spending_score variables did age reveal a positive 
# relationship with loyalty_points. This is likely due to omitted variable bias.
# Omitted variable bias (OVB) occurs when the regressor, X, in this case 'age',
# is correlated with an omitted variable and the omitted variable is a determinant
# of the dependent variable, Y, in this case loyalty_points. 

# Determine the correlation between variables

numeric_vars <- df[sapply(df, is.numeric)]
# remove "product" from numeric_vars
numeric_vars <- subset(numeric_vars, select = -product)
# Create correlation_matrix of relevant variables
correlation_matrix <- cor(numeric_vars)
correlation_matrix
# Visualise correlation_matrix (make it look pretty)
install.packages("psych")
library(psych)
corPlot(numeric_vars, cex=2)

# Correlation between age and spending_score is -0.22, indicating a negative 
# and significant correlation between the variables. spending_score is also the 
# strongest predictor of loyalty_points with a correlation of 0.67. Therefore, 
# including spending_score removed OVB. remuneration has a strong correlation 
# with loyalty_points and almost no correlation with age. 

# Print the summary statistics
summary(model)

# All features are significant to the one-degree significance level. The model
# achieves an R-squared of 0.8399 and an adjusted R-squared of 0.8397, implying
# high explanatory power of the model with no evidence of over-fitting. 

# Create a new model with "product" variable 

model2 <- lm(loyalty_points ~ age+remuneration+spending_score+product, data=df)
model2
summary(model2)

# product proved to be a statistically insignificant variable with a p-value of 
# 0.75 and there was no improvement in the R-squared score, meaning the model is
# made worse by the feature's inclusion. 

# Comment: Although "product" appeared to show no meaningful explanatory power, 
# this could just be due to how the data has been stored. For example, if we 
# created a variable called "product_category", which grouped the products into
# types, such as board games, we might be able to uncover behavioral trends among
# customers. This could be something to explore in future research. 

# Create a new model without 'age' to see if it is worth keeping in the model

model3 <- lm(loyalty_points ~ remuneration+spending_score, data=df)
model3
summary(model3)

# Comment: Removing age from the model lowers the R-squared and adjusted R-squared,
# meaning that the model is made slightly better by including age. Therefore, the
# first model, 'model', is the best choice. 