#PROJECT TITLE - COVID19 ANALYSIS

# Load packages
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

# (A) LOAD THE DATA
#Read in datasets/WHO_COVID_19.csv


WHO_COVID_19 <- read_csv("https://covid19.who.int/WHO-COVID-19-global-table-data.csv")

WHO_COVID_19

WHO_COVID19

#Rename global datasets
WHO_COVID19 <- WHO_COVID_19_global_data_6_
WHO_COVID19

# EXAMINE THE DATA
head(WHO_COVID19)

# (B) CLEANING THE DATASETS
# Checking data types

is.numeric(WHO_COVID19$New_cases)
#this confirms that the 'New Cases' column is numeric

is.numeric(WHO_COVID19$Country)
#this confirms that the 'Country' column is non-numeric

#Search for full duplicates in datasets
sum(duplicated(WHO_COVID19))
#this confirms there are no duplicates in the datasets

#Search for partial duplicates in datasets
# i) Remove full and partial duplicates
distinct(WHO_COVID19, keep_all = TRUE)

#Use function to repeat search for duplicates in datasets *****FUNCTION *****
# Removing full duplicates - using function
clean_data_nofullduplicates <- function(filename) {
  distinct(filename, keep_all = TRUE)
}

clean_covid_data <- clean_data_nofullduplicates(WHO_COVID19)
clean_covid_data

  
# iii) Removing partial duplicates - Filter for rows with count > 1 
#First identify any partial duplicates, next drop all partial duplicates, keeping only the first

WHO_COVID19 %>%
  count(Cumulative_cases) %>% #counts number of occurrences of cumulative cases
  filter(n > 1)  #filters for rows with a count > 1.
#this suggests that 5,248 rows of partial duplicates have been filtered out

#iii) Repeat search for partial duplicates using New cases
WHO_COVID19 %>%
  count(New_cases) %>%
  filter(n > 1)

#Use function to repeat search for duplicates in datasets *****FUNCTION *****
# Removing partial duplicates - using function
clean_data_nopartialduplicates <- function(filename) {
  count(filename) %>%
  filter(n >1)
}

clean_covid_data_nopartialdup <- clean_data_nopartialduplicates(WHO_COVID19)
clean_covid_data_nopartialdup

# (C) - EXPLORATORY DATA ANALYSIS

# i) Confirmed cases worldwide - lineplot
ggplot(WHO_COVID_19_global_data_6_, aes(x=Date_reported, y=Cumulative_cases)) + 
  geom_line() + ylab("Cumulative Cases") + ggtitle("Global Cumulative Cases")

# ii) Run levels on all columns in the WHO_COVID datasets
levels(WHO_COVID19$Country)
levels(WHO_COVID19$WHO_region)
levels(WHO_COVID19$New_cases)
levels(WHO_COVID19$Cumulative_cases)
levels(WHO_COVID19$New_deaths)
levels(WHO_COVID19$Cumulative_deaths)

#Top 10 countries hardest hit by the pandemic by early Nov 2020
# iii) Groupby Country and summarise total new CASES Globally
Top10Cases_COVID19 <- WHO_COVID19 %>%
  group_by(Country) %>%
  summarise(total_cases = max(Cumulative_cases)) %>%
  top_n(10, total_cases)

Top10Cases_COVID19    

# iv) Groupby Country and summarise total new DEATHS Globally
Top10Deaths_COVID19 <- WHO_COVID19 %>%
  group_by(Country) %>%
  summarise(total_deaths = max(Cumulative_deaths)) %>%
  top_n(10, total_deaths) 

# see the result
Top10Deaths_COVID19

#USE VISUALISATIONS TO SHOW NEW CASES AND NEW DEATHS GRAPHICALLY

# v) Filter out and drop levels to focus on WHO_region datasets of EURO alone
WHO_COVID19_EURO <- WHO_COVID_19_global_data_6_ %>%
  filter(WHO_region == "EURO")

# see the result
WHO_COVID19_EURO

# vi) Select columns to remove Country Code (but keep all other columns)
WHO_COVID19_EURO %>%
  select(-Country_code)

# Top 5 EURO countries hardest hit by pandemic by early Nov 2020
# vii) Groupby Country and summarise total new EURO cases
Top5Cases_WHO_COVID19_EURO <- WHO_COVID19_EURO %>%
  group_by(Country) %>%
  summarise(total_cases = max(Cumulative_cases)) %>%
  top_n(5, total_cases)


# see the result
Top5Cases_WHO_COVID19_EURO
nrow(Top5Cases_WHO_COVID19_EURO)


#viii) Groupby Country and summarise total new EURO deaths
Top5Deaths_WHO_COVID19_EURO <- WHO_COVID19_EURO %>%
  group_by(Country) %>%
  summarise(total_deaths = max(Cumulative_deaths)) %>%
  top_n(5, total_deaths)

# see the result
Top5Deaths_WHO_COVID19_EURO


#Visualise plot of EURO Countries with Cumulative Cases
ggplot(WHO_COVID19_EURO, aes(Date_reported, New_cases)) + geom_line() +
  facet_wrap(~Country) + ylab("Cumulative New Cases") + ggtitle("Distribution of
  Covid cases per European country - to early Nov 2020")

#Filter EURO datasets for Germany only
WHO_COVID19_Germany <- WHO_COVID19_EURO %>%
  filter(Country == "Germany")

# see the result
WHO_COVID19_Germany

#CREATE FILTER FUNCTION TO FILTER EUROPEAN DATASETS FOR "COUNTRY"
filter_country <- function(filename) {
  filter(Country)
}
#**** THIS FUNCTION MIGHT NOT WORK AS I'D NEED TO PROVIDE DIFFERENT COUNTRIES WITHIN SAME DATASET!
#eg filter(country == "Germany"). How do i rep this detail within a function?
#Filter EURO datasets for UK only
WHO_COVID19_UK <- WHO_COVID19_EURO %>%
  filter(Country == "The United Kingdom")

# see the result
WHO_COVID19_UK


#Filter EURO datasets for Sweden only
WHO_COVID19_Sweden <- WHO_COVID19_EURO %>%
  filter(Country == "Sweden")

# see the result
WHO_COVID19_Sweden

#Filter EURO datasets for Germany, UK and Sweden combined
WHO_COVID19_GUS <- WHO_COVID19_EURO %>%
  filter(Country == "Germany" | Country == "The United Kingdom" | Country == 
  "Sweden")

# see the result
WHO_COVID19_GUS

# Visualise plots of New Cases in Germany, UK and Sweden (with facets)
WHO_COVID19_GUS %>%
  ggplot(aes(Date_reported, New_cases)) + geom_line() + facet_wrap(~Country) + 
  ggtitle("Distribution of new Covid cases in Germany, UK & Sweden Jan - Nov 
  2020")

#Visualise plots of Cumulative Cases in Germany, UK and Sweden (with facets)
WHO_COVID19_GUS %>%
  ggplot(aes(Date_reported, Cumulative_cases)) + geom_line() + 
  facet_wrap(~Country) + ggtitle("Distribution of cumulative Covid cases in 
  Germany, UK and Sweden Jan - Nov 2020")

#Visualise plots of new covid Deaths in Germany, UK and Sweden (with facets)
WHO_COVID19_GUS %>%
  ggplot(aes(Date_reported, New_deaths)) + geom_line() + facet_wrap(~Country) +
  ggtitle("Distribution of new Covid deaths in Germany, UK and Sweden Jan - Nov
  2020")

#Visualise plots of cumulative covid deaths in Germany, UK and Sweden (facets)
WHO_COVID19_GUS %>%
  ggplot(aes(Date_reported, Cumulative_deaths)) + geom_line() + 
  facet_wrap(~Country) + ggtitle("Distribution of cumulative Covid deaths in 
  Germany, UK and Sweden Jan - Nov 2020")

# INVESTIGATE FOR OUTLIERS
#Generate boxplots for New Cases for Germany, UK and Sweden
WHO_COVID19_GUS %>%
  ggplot(aes(Country, New_cases)) + geom_boxplot() + ggtitle("Trends in mean 
  and median New Covid Cases in Germany, UK and Sweden")

#Generate boxplots for New Deaths for Germany, UK and Sweden 
WHO_COVID19_GUS %>%
  ggplot(aes(Country, New_deaths)) + geom_boxplot() + ggtitle("Trends in mean 
  and median New Covid Deaths in Germany, UK and Sweden")

#Box plot for New Cases in GUS with scale_y_log10()
WHO_COVID19_GUS %>%
  ggplot(aes(Country, New_cases)) + geom_boxplot() + scale_y_log10() + ggtitle(
  "mean & median New Covid Cases in Germany, UK and Sweden, scale_y_log10()")


# 5 quantile summaries of cases in GUS show median values of New cases of 1000
# in Germany, approx 650 in Sweden and approx 2500 in UK. Also outliers extending
# >1.5 greater than the IQR below Q1 (rep in circles for GUS i.e (Q1 - 1.5*IQR)

#Histogram plot for New Cases in Germany
WHO_COVID19_Germany %>%
  ggplot(aes(New_cases)) + geom_histogram() + 
    ggtitle("Histogram plot of New Cases in Germany")

#Histogram plot for New Cases in UK
WHO_COVID19_UK %>%
  ggplot(aes(New_cases)) + geom_histogram() + 
  ggtitle("Histogram plot of New Cases in the UK")

#Histogram plot for New Cases in Sweden
WHO_COVID19_Sweden %>%
  ggplot(aes(New_cases)) + geom_histogram() + 
  ggtitle("Histogram plot of New Cases in Sweden")

#all histograms for GUS show right skews as the values on high end are more
#spread out than the values on the low end (left side)

#Standard Deviation Values for each GUS Country
sd(WHO_COVID19_Germany$New_cases)
#sd for Germany = 2967.891

sd(WHO_COVID19_UK$New_cases)
#standard deviation for UK = 5566.717

sd(WHO_COVID19_Sweden$New_cases)
#standard deviation for Sweden = 446.3435
#sd indicates on average, how much each value differs from the mean. It estimates
#how extreme a given value is under assumption it came from a normal distribution.
#The 68-95-99.7 rule states that 68% of values in normal dist fall within 1s.d of mean
#while 95% and 99.7% of values fall within two and 3 s.d respectively.

#Before fitting a regression model to data, it can be useful to determine how the 
#independent variables are related to the dependent variable & to each other.
#CORRELATIONS - measuring this between two variables gives a way to quickly check 
#for linear relationships among independent variables and the dependent variable.
#For Germany
cor(WHO_COVID19_Germany$New_cases, WHO_COVID19_Germany$New_deaths)
#0.306299
cor(WHO_COVID19_Germany$Cumulative_cases, WHO_COVID19_Germany$New_deaths)
#0.3826549

#For UK
cor(WHO_COVID19_UK$New_cases, WHO_COVID19_UK$New_deaths)
#0.2161739
cor(WHO_COVID19_UK$Cumulative_cases, WHO_COVID19_UK$New_deaths)
#-0.05927264 (the -ve correlation implies that increase in cum cases leads to decreases in deaths)

#For Sweden
cor(WHO_COVID19_Sweden$New_cases, WHO_COVID19_Sweden$New_deaths)
#0.2789365
cor(WHO_COVID19_Sweden$Cumulative_cases, WHO_COVID19_Sweden$Cumulative_deaths)
#0.939301

# CREATE MODELS
#Baseline model - specify 60/40 split
data_sample <- sample(c(TRUE, FALSE), nrow(WHO_COVID19_GUS), replace = T, prob = 
  c(0.6,0.4))

train <- WHO_COVID19_GUS[data_sample, ]
test <- WHO_COVID19_GUS[data_sample, ]
# this code worked once and is no longer working

# Fit a linear regression model using Cumulative_cases 
covid_model <- lm(New_deaths ~ Cumulative_cases, data = WHO_COVID19_GUS)

#Generate covid_model outputs
covid_model
#explanation - the coefficient for Cumulative cases is +ve so New Deaths 
#increases as Cumulative cases increases as well

summary(covid_model)

glance(covid_model)

wrapFTest(covid_model)


#Predicting using covid_model (INCORRECT!)
WHO_COVID19_GUS$prediction <- predict(covid_model)
#THIS IS NOT CORRECT AS YOU NEED A TEST DATASET ON WHICH TO TEST THE MODEL'S 
#PREDICTIONS. NEED TO INCLUDE THE TEST DATASET WITHIN THE PREDICT() FUNCTION

#Looking at the Predictions - compare model's predictions to the actual data
WHO_COVID19_GUS %>%
  ggplot(aes(prediction, New_deaths)) + geom_point() + geom_abline(color = 
  "darkblue") + ggtitle("New Deaths vs linear model prediction")
# graph giving visual picture of how close the model's predictions are to ground
#truth)

summary(covid_model)
#Cumulative_cases estimate of 1.027e-04 shows that for each additional cumulative
#case, we'd expect 1.027e-04 additional New death on average (if all else held constant)

#EVALUATING MODEL PERFORMANCE
#i)RESIDUALS SECTION - as residual equal to true value minus predicted value, the 
#max error of 1156.68 suggests the model under-predicted by approx 1150 for at 
#least one observation. On other hand, 50% of errors fall within the 1Q and 3Q
#values so the majority of predictions were btwn 18.88 and 61.96 over true value 

#ii) For each estimated regression coeff, the p-value (Pr>|t|), provides an estimate
#of prob that true coeff is 0 given the value of estimate. p-value of 0.0019 IS 
#statistically sig (as it's < sig level). This indicates that the feature 
#Cumulative_cases IS predictive of the outcome. 

#iii) The Multiple R-sq value (aka coeff of determination) provides measure of
#how well the model as a whole explains the values of the dep variable. Similar 
#to the corr coeff in that the closer to 1.0, the better the model. R-sq here is 
#0.94% which means that the model explains only 0.94% of the variation in the dep
#variable

#CONCLUSION - given the 3 performance indicators above, our model is not performing 
#so well (mainly on the R^2)

#Fit a linear regression model using New_cases
covid_model1 <- lm(New_deaths ~ New_cases, data = WHO_COVID19_GUS)

summary(covid_model1)
#This shows an improvement on the metrics above, indicating that New_cases is more 
#highly predictive of New_deaths outcome than Cumulative_cases, with a p-value of
#2e-16. Also an adjusted R-sq value of 0.08874 (i.e. 8.874%) which means that the
#model explains 8.8% of the variation in the dep. variable. This makes sense as 
#the causative factors strongly related to age, DM, HBP, Respiratory D etc.These
#would make up the remaining (100 - 8.874)%


#Fit a linear regression model using New_cases, Cumulative_cases, Cum_Deaths
covid_model2 <- lm(New_deaths ~ New_cases * Cumulative_cases * Cumulative_deaths, 
    data = WHO_COVID19_GUS)

summary(covid_model2)
#Residual - This shows the greatest improvement, with a max Residual error of 709.76 suggesting
#the model underpredicted by 709 cases for at least 1 observation. With 50% of errors
#between 1Q and 3Q, this shows the majority of predictions were between 51.97 over 
#true value and 26.95 under true value.

# p-values all statistically significant here (< 0.05)

#Multiple R^2 value shows an R of 57.67% meaning that the model explains nearly 
#60% of the variation in the dependent variable.

# MAKING PREDICTIONS WITH THE COVID_MODEL2 REGRESSION MODEL
#Next, we'd like to use the model to predict the likelihood of future Deaths.
#First apply the model to the original training data using the predict() function

WHO_COVID19_GUS$pred <- predict(covid_model2, WHO_COVID19_GUS)
#This will save the predictions as a new vector named pred in the WHO dataset.
#We can then compute the correlation between the predicted and actual number of 
#new deaths
cor(WHO_COVID19_GUS$pred, WHO_COVID19_GUS$New_deaths)
0.7615347 #this suggests a very strong linear relationship between predicted and
#actual values. This is a good sign and suggests the model is highly accurate.

#Also useful to plot above as a scatterplot
WHO_COVID19_GUS %>% 
  ggplot(aes(pred, New_deaths)) + 
  geom_point() +geom_abline(colour = "red") +
  ggtitle("New Deaths vs Linear Model Predictions2")

#The off diagonal points falling above the line are cases where the actual number 
#of New deaths was greater than expected, while cases falling below the line are
#those less than expected.


#Now use model to forecast New Death rates for France
#Firstly, filter EURO datasets for France alone

WHO_COVID19_France <- WHO_COVID19_EURO %>%
  filter(Country == "France")

#Now, predict for France
pred_france <- predict(covid_model2, WHO_COVID19_France)

pred_france
summary(pred_france)


#CREATE MODELS - USING TEST/TRAIN SPLIT (*******CORRECT!!!!!******)
#Use nrow to get the number of rows in WHO_GUS

N <- nrow(WHO_COVID19_GUS)

N #(915 rows)

#Calculate how many rows 75% of N should be. Use round() to get integer
target <- round(N * 0.75)  
target # (686 rows)

#Create a vector of N uniform random variables:
gp <- runif(N)

#Use gp to create the training set: covid_train(75% of data) and covid_test(25%)
covid_train <- WHO_COVID19_GUS[gp < 0.75, ]
covid_test <- WHO_COVID19_GUS[gp >= 0.75, ]

#Use nrow() to examine covid_train and covid_test
nrow(covid_train) #679
nrow(covid_test)  #236

#Now that we have split the WHO_GUS dataset into covid_train and covid_test, use 
#covid_train to train the model to predict future covid deaths

covid_model3 <- lm(New_deaths ~ New_cases * Cumulative_deaths, data = covid_train)

#Use summary to examine the model
summary(covid_model3)

#Try to train another model using New_cases, Cum_cases and Cum_deaths this time
covid_model4 <- lm(New_deaths ~ New_cases * Cumulative_cases * Cumulative_deaths, data = covid_train)

#Use summary to examine the model
summary(covid_model4)

#Summary covid models 3 & 4
#Residuals - covid_model4 is the better of the two models here with a max Residual
#error of 654.82 suggesting the model is underpredicting by this number of cases vs
# model3 which is doing same by 1067.62 cases. With 50% of errors between 1Q and 
#3Q, this shows the majority of predictions were between 48.88 over & 27.29 under
#true value

# p-values all statistically significant for both models (< 0.05)

#Multiple R^2 shows a value of 14.5% for model 3 vs 62.5% for model 4, showing
#that model4 explains over 62% of the variation in the dependent variable but model 3
# only explains 14.5% of the dependent variable 

# MAKING PREDICTIONS WITH THE COVID_MODEL4 REGRESSION MODEL
#Next, we'd like to use the model to predict the likelihood of future Deaths. 
#First apply the model to the original training data using the predict() function

covid_train$pred <- predict(covid_model4) #as simply calling predict() on the model
#returns the predictions for the data you used to fit the model i.e. the training data.
#HEre, i called predict on the covid model and added a column of predictions to 
#the training dataframe. We can compare the model's prediction on (x-axis) to actual
#New deaths in the data on (y-axis). If the model predicted perfectly, wld see a 
#straight line through origin graph with all points lying on the line. Graph gives
#visual idea of how close model's predictions are to ground truth. 

#To apply the model to new data, use the argument 'newdata'. Below, have a new 
#dataframe of COVID observations called 'covid_test'. To apply the model to covid_test
#and add predictions as a new column of the dataframe. 

#Predict New_deaths from WHO_GUS for the test set
covid_test$pred <- predict(covid_model4, newdata = covid_test)

#Evaluate the rmse on both training and test data and print them (*Look in book - this is no longer working)
rmse_train <- rmse(covid_train$pred, covid_train$New_deaths)
rmse_train

rmse_test <- rmse(covid_test$pred, covid_test$New_deaths)
rmse_test 
#**UNABLE TO CALCULATE RMSE USING THIS METHOD**

#USING DIFFERENT METHOD TO CALCULATE RMSE FOR MODEL (*** SEE BELOW - AFTER RESIDUAL CALCULATION)


#Evaluate the r-squared on training and test data and print
rsq_train <- rsq(covid_train$pred, covid_train$New_deaths)
#UNABLE TO CALCULATE R-SQUARED FOR TRAINING AND TEST DATA. ?PACKAGE. USE R^2
#OBTAINED FOR THE MODEL 

#EXPLANATION OF RMSE FOR TRAINING AND TEST DATA
#Similar output values on test and training data (as seen with RMSE) suggests a 
#good model performance. Generally model performance is better on training than on test data

#Plot the covid_model4 predictions(x-axis) against outcome(New_Deaths) on TRAINING data

covid_train %>%
  ggplot(aes(pred, New_deaths)) + geom_point() + geom_abline(color = "blue") + 
  ggtitle("New Deaths vs Covid Linear Model Predictions on TRAINING data")
#Interpretation - some points entirely above & below line of perfect prediction indicating
#that the model doesn't fit the training data very well. Showing SYSTEMATIC ERRORS 
#suggesting that i don't yet have all of the relevant/important independent variables to fit
#my model OR that i need an algorithm that can find more complex relationships in the data.
#although it can be argued that the residuals here ARE evenly distributed above and
#below the best fit line suggesting that the systematic errors are ?minimal

#Another way to evaluate model predictions besides comparing to ground truth (as above)
#is to examine prediction error. To do this, calculate Residuals and plot against 
#Predictions. Residuals = actual outcome - predicted outcome (*** DID NOT WORK***)

#From previous step
covid_train$pred <- predict(covid_model4)

#Calculate residuals
covid_train$residuals <- covid_train$New_deaths - covid_train$pred

#Fill in the blanks to plot predictions (x-axis) vs residuals (***DID NOT WORK***)
ggplot(WHO_COVID19_GUS, aes(pred, residuals)) + geom_pointrange(aes(ymin = 0, 
  ymax = residuals)) + geom_hline(yintercept = 0, linetype = 3) + ggtitle("residuals vs linear model prediction")

#CALCULATING RMSE USING RESIDUALS

#calculate error
err <- covid_train$pred - covid_train$New_deaths
err

#Square the error vector
err2 <- err^2

#Calculate RMSE - take the mean and sqrt it
(rmse <- sqrt(mean(err^2)))
#answer of 110.2363

#Calculate the standard dev of New deaths
(sd_newdeaths <- sd(covid_train$New_deaths))
#answer of 175.9995

#An RMSE much smaller than the outcome's standard deviation suggests a model that 
#predicts well. 

#Calculate the R^2 of the covid prediction model

# i) Calculate error
err <- covid_train$pred - covid_train$New_deaths

# ii) Square it and take the sum
rss <- sum(err^2)

# iii) Take the difference in number of deaths from the mean number
toterr <- covid_train$New_deaths - mean(covid_train$New_deaths)

# iv) Square it and take the sum
sstot <- sum(toterr^2)

rss
sstot

r_sq <- 1 - (rss/sstot)

r_sq
#THE R-SQUARED IS 0.6071184. It can also be viewed by calling summary() or glance()
#on an lm model but not all regression algos in R return it so good to know how to c
#calculate it yourself. It gives an estimate of degree of correlation between model 
#prediction and true outcome. Aka, how well the model fits the training data.
#Only true for training data, not for new data.


#TO COMPARE BETWEEN THE TWO, CALCULATE RMSE AND R^2 FOR MODEL FOR BOTH TRAINING AND TEST SETS
# I.E. do same as above for the model using the 'covid_test' dataset.
#If the model performance (RMSE and R^2 is not too diff between training and test sets
#it means the model is NOT overfit). i.e. similar RMSE and R^2 values on TEST 
#datasets is even more proof that a model is working as well as expected. 


#Plot the covid-model4 predictions(on the x-axis) against the outcome(New_deaths) on TEST data
covid_test %>%
  ggplot(aes(pred, New_deaths)) + geom_point() + geom_abline(color = "red") + 
    ggtitle("New Deaths vs Covid Linear Model Predictions")



#ALTERNATIVE MODEL TO PREDICT NEW DEATHS - NUMERIC DECISION (REGRESSION) TREE MODEL
#Using regression trees (& model trees), we may be able to improve the regression 
#results (c.f. linear regression model) while still having a model that is easy to understand.

#TRAINING A REGRESSION TREE MODEL ON THE DATA (train/test data split)
#Building the model: (call this "m.rpart" in future)

m <- rpart(New_deaths ~ New_cases + Cumulative_cases + Cumulative_deaths, 
           data = covid_train)

#Making Predictions: (call this "p.rpart" in future)

p <- predict(m, covid_test, type = "vector")

p
summary(p)

#Visualise the decision tree diagram
rpart.plot(m, digits = 3)

#Interpretation - the numbers shown in the leaf nodes are the predicted values 
#for the examples reaching that node. This highlights that of the 3 independent 
#variables, New Cases is the most crucial factor in predicting New Covid deaths.


#Evaluating model performance
#Already used regression tree model to make predictions (p) above.

#Comparing the summary of our predictions against that of true values

summary(p)

summary(covid_test$New_deaths)

#Explanation - this finding suggests that the model IS correctly identifying extreme
#cases (with both Max values quite similar). On the other hand, it's not doing a 
#great job between the 1st and 2nd(Median) quartiles.

#Calculate correlation between predicted and true values
cor(p, covid_test$New_deaths) #0.8482374 i.e. 84% correlated
#This correlation measures how strongly predictions are related to the true value

#MEasuring model performance with the mean absolute error
#another way to think about the model's performance is to consider how far, on 
#avg, its prediction was from the true value. This measurement is called the MAE

#Create simple MAE function():

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

#Then the MAE for predictions here will be:
MAE(p, covid_test$New_deaths) #39.90
#This implies that on average, the difference between our model's predictions and 
#the true quality score was about 39.90 (c.f. his eg of 0.59). On a quality scale 
#from 0 to 10, this seems to suggest our model is not doing too well. This suggests
#that there is room for improvement. 

#Improving Model Performance
#To improve performance of the learner algo, we apply a model tree algo, which is 
#a more complex application of trees to numeric prediction. A model tree extends 
#regression trees by replacing the leaf nodes with regression models. This often
#results in more accurate results than regression trees, which use only a single 
#numeric value for the prediction at the leaf nodes. 

m.cubist <- cubist(x = covid_train[-10], y = covid_train$New_deaths)
#UNABLE TO FIT CUBIST MODEL TREE USING CODE IN PAGE 213


# (H) NEED TO INCLUDE INDEPENDENT VARIABLES FROM DATE REPORTED COLUMN

# Generate the 2 new date/time variables
WHO_COVID19_GUS_DATES <- WHO_COVID19_GUS %>%
  mutate(wday = wday(Date_reported, label = TRUE), month = month(Date_reported,
  label = TRUE))

WHO_COVID19_GUS_DATES

# CREATE REGRESSION MODELS INCLUDING USE OF DATES & INTERACTIONS

#Build formula to express WHO_COVID as a function of all independent variables: fmla
(fmla <- as.formula("New_deaths ~ Cumulative_cases + wday + month +
    Cumulative_cases:Cumulative_deaths"))

#model.matrix() converts categorical variables with N levels into N - 1 indicator 
#variables. All become numerical values.
#Use fmla and model.matrix to see how the data is represented for modeling

mmat <- model.matrix(fmla, WHO_COVID19_GUS_DATES)

#Examine the first 20 lines of mmat
head(mmat, n = 20)
#Shows how most R functions represent the categorical variables internally for modeling 

#Fit a model to predict New Deaths from the independent variables

covid_model5 <- lm(fmla, data = WHO_COVID19_GUS_DATES)
#***NOTE WHEN RE-DOING THIS, TO SPLIT THE DATASETS ABOVE INTO TRAINING AND TEST SETS FIRST***

#Summary to examine covid_model5
summary(covid_model5)


# Build new formula to include New cases (*** 2ND ATTEMPT TO INCLUDE NEW CASES AND IMPROVE R^2)
(fmla1 <- as.formula("New_deaths ~ Cumulative_cases + wday + month + New_cases +
    Cumulative_cases:Cumulative_deaths"))
#Cumulative cases and deaths represented in this way because the simultaneous effect
#on outcome is NOT additive - an INTERACTION exists here. Use other models that can 
#learn about the interactions from the data directly so you don't have to encode these yourself.


#FIT A MODEL TO PREDICT NEW DEATHS FROM INDEP VARIABLES INCLUDING NEW CASES
#NOTE - not built model matrix for this process

covid_model6 <- lm(fmla1, data = WHO_COVID19_GUS_DATES)

#summary to examine covid_model6
summary(covid_model6)

#INTERPRETATION - only slight improvement by adding new cases as indep variable 
#suggesting that other relevant indep variables are required to predict risk of 
#covid deaths. also possibly that a non-linear relationship exists and other models might be better.

#PRedict the number of New deaths using the model
WHO_COVID19_GUS_DATES$predictions <- predict(covid_model5)

#Plot predictions vs actual New deaths
WHO_COVID19_GUS_DATES %>%
  ggplot(aes(predictions, New_deaths)) + geom_point() + geom_abline(colour = "red") +
  ggtitle("Plot of linear model predictions")

#WE have fit a linear regression model using categorical variables & seen how cat variables are rep
#in the model.

#NEXT, FIT A GENERALISED ADDITIVE MODEL USING SAME INDEP VARIABLES. Using GAM as
# i suspect a non-linear relationship exists between Cumulative Cases, Cumulative
#Deaths and New Deaths. Therefore use s(Cumulative_cases) & s(Cumulative_deaths)
#Using this b'cos i believe New deaths is not well described by a linear model.

#Build formula to test in GAM (IN FUTURE, USE UNDERSCORES, EG fmla_gam)

fmla.gam <- New_deaths ~ wday + month + s(Cumulative_cases) + s(Cumulative_deaths)
#formula above states to model wday & month as linear relationships but the rest as non-linear

#Fit the GAM Model (****DON'T FORGET TO TUNE THE MODEL - ELSE PPL WILL SAY YOU DON'T UNDERSTAND ML)

model.gam <- gam(fmla.gam, data = WHO_COVID19_GUS_DATES, family = gaussian)

#Call summary() on covid_model5 and look for R^2
summary(covid_model5)

#Call summary() on model.gam and look for R^2
summary(model.gam)

#Predict New Deaths using GAM model
WHO_COVID19_GUS_DATES$gampredictions <- predict(model.gam)

#Plot predictions vs actual new deaths
WHO_COVID19_GUS_DATES %>%
  ggplot(aes(gampredictions, New_deaths)) + geom_point() + geom_abline(color = "blue") +
  ggtitle("Plot of generalised additive model predictions")

#You have fit a generalised additive model. For this data, the GAM appears to fit 
#the data better than a linear model, as measured by the R^2 (for GAM = 85.6%) vs 
#for linear model (R^2 = 44.5%)

#PREDICT WITH THE MODEL ON TEST DATA 
#Here, apply the covid models (covid_model5 and model.gam) to new data - covid_test

#BUT FIRST SPLIT THE DATASETS INTO TRAINING AND TEST SETS FIRST

# Create Vector of N uniform random variables 
gp <- runif(N)

#Use gp to create the training set: covid_train1(75%) and covid_test1(25% of data)
covid_train1 <- WHO_COVID19_GUS_DATES[gp < 0.75, ]
covid_test1 <- WHO_COVID19_GUS_DATES[gp >= 0.75, ]

#Use nrow to examine covid_train and covid_test
nrow(covid_train1) 
nrow(covid_test1)

#Get predictions from linear model
covid_test1$pred.lin <- predict(covid_model5, newdata = covid_test1)

#Get predictions from gam model
covid_test1$pred.gam <- predict(model.gam, newdata = covid_test1)

#Gather the predictions into a "long" dataset
covid_long <- covid_test1 %>%
  gather(key = modeltype, value = pred, pred.lin, pred.gam)

#Calculate the rmse
covid_long %>%
  mutate(residual = New_deaths - pred) %>% #residuals
  group_by(modeltype) %>% #groupby modeltype
  summarise(rmse = sqrt(mean(residual^2))) #calculate rmse

#Compare the predictions against actual New_deaths on the test data (*INCORRECT - SEE 
#PREDICT WITH THE SOYBEAN MODEL ON TEST DATASET).
covid_test1 %>%
  ggplot(aes(x=Cumulative_cases)) + geom_point(aes(y=New_deaths,)) +  
  scale_color_brewer(palette="Dark2") + geom_abline(color = "red")

#Build a random forest model for New Deaths

#Random seed to reproduce results
seed #this didn't work

#The outcome column
(outcome <- "New_deaths")

# THe input variables 
(vars <- c("Cumulative_cases", "Cumulative_deaths", "wday", "month"))

#Create the formula string for New deaths as a function of the inputs
(fmla2 <- paste("New_deaths", "~", paste(vars, collapse = " + ")))

#Fit and print the random forest model (****DON'T FORGET TO TUNE THE MODEL AS WELL AFTERWARDS)
(covid_model_rf <- ranger(fmla2, covid_train1, num.trees = 500, 
                          respect.unordered.factors = "order"))
#**code above should include seed = seed)) at the end - to reproduce results, but didn't work**

#INTERPRETATION - WE HAVE FIT A MODEL TO THE DATA WITH A RESPECTABLE R^2 (87.45%)

#Next, let's see how well model fit above performs on holdout data (i.e. predicts 
#using our covid_test1 dataset)

covid_model_rf

#Make predictions on the covid_test1 dataset
covid_test1$pred <- predict(covid_model_rf, covid_test1)$predictions

covid_test1$pred

#Calculate the RMSE of the predictions
covid_test1 %>%
  mutate(residual = New_deaths - pred) %>%
  summarise(rmse = sqrt(mean(residual^2)))

#Plot actual outcome vs predictions (predictions on x-axis)
ggplot(covid_test1, aes(x=pred, y=New_deaths)) + geom_point() + geom_abline() +
  ggtitle("Random Forest plot of predictions vs New deaths")


#Function to load in WHO_COVID file
load_file <- read_csv("~/downloads/WHO_COVID_19_global_data_6_.csv")

load_file <- read_csv("WHO_COVID_19_global_data_6_")
#DID NOT WORK BECAUSE FILE IS NOT IN CURRENT WORKING DIRECTORY

load_file <- function(filename) {
  WHO_COVID19 <- read_csv("~/Downloads/WHO_COVID_19_global_data_6_.csv")
}

load_file()
#CREATE A CROSS VALIDATION PLAN

#Get the nnumber of rows in the dataset

nRows <- nrow(WHO_COVID_19_global_data_6_)
nRows # 71675

#Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(71675, 3, NULL, NULL)

#Examine the split plan
str(splitPlan)

#You have created a 3-way cross validation plan. Next, you will use this plan to 
#evaluate a "potential" model

#Use the "Evaluate a modeling procedure using n-fold cross validation section" next

#Remember cross validation validates the "modeling process" not an actual model (
#which a "test-train split" can do


gam_model_UK <- gam(Cumulative_deaths ~ s(Cumulative_cases, data = covid_train1))
