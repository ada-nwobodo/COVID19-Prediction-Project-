#!/usr/bin/env python3

#This script pulls in WHO datasets for New cases and new deaths from COVID-19, 
#builds and tests several predictive models, and subsequently makes predictions 
#for mortality rates. A comparison is done between lockdown and social distancing 
#approaches to the pandemic using the best model trained from the data.

#__author__ = 'Dr Adaeze Nwobodo'
#__email__ = 'AdaezeNwobodo@gmail.com'


#import library packages {caret/Dplyr/lubridate/mgcv/purrr/ranger/rpart/
#rpart.plot/sigr/tidyselect/tidyverse/wrapr/base/broom/cluster/datasets/ellipsis
#/ggplot2/graphics/grDevices/highr/lattice/maggritr/methods/nlme/readr/readxl/
#renv/stats/stringr/tibble/tidyr/utils}

#DEFINE HELPER FUNCTIONS
load_file <- function(filename) {
  #loads csv into a dataframe
  read_csv(filename)
}


clean_data_nofullduplicates <- function(data_frame) {
  #removes rows that contain full duplicates. Keep all non-duplicates
  distinct(data_frame, keep_all = TRUE)
}


clean_data_nopartialduplicates <- function(data_frame) {
  #removing partial duplicates
  count(data_frame) %>%
    filter(n > 1)
}


clean_data_noCountryCode <- function(data_frame) {
  #removes country code column as not useful to exploration, prediction project
  select(data_frame, -Country_code)
}


get_levels <- function(data_frame) {
  #check for levels within all datasets
  levels(dataframe)
}


get_wday_month <- function(data_frame) {
  #obtain weekday & month datasets from 'Date_reported' column for use as variables
  data_frame %>%
  mutate(wday = wday(Date_reported, label = TRUE), month = month(Date_reported, 
  label = TRUE))
}


calculate_test_train_split <- function(data_frame) {
  #split the datasets into training(0.75) & test(0.25) to train and test models
  N <- nrow(data_frame) %>%
  round(N * 0.75) %>%
  gp <- runif(N) %>%
  covid_train <- data_frame[gp < 0.75, ]
  covid_test <- data_frame[gp >= 0.75, ]
}


train_model_lm <- function(data_frame) {
  #train specific model-types using Country-specific datasets eg WHO_COVID19_UK_dates 
  #for subseq prediction
  (vars <- c("Cumulative_cases", "Cumulative_deaths", "wday", "month"))
  lm(New_deaths ~ vars, data = data_frame)
}


train_model_outputs <- function(model, train_df, test_df, num_procs, rmse, cv_std, cv = 2) {
  #trains all different model types (lm, random forest etc for comparable outputs)
  neg_mse = cross_val_score(model, train_df, test_df, cv = 2, n_jobs = num_procs, 
                            scoring = "neg_mean_squared_error")
  mean_mse[model] = -1.0*np.mean(neg_mse)
  cv_std[model] = np.std(neg_mse)
  RMSE[model] = sq.rt(mean(err ^ 2)) #error = WHO_COVID19$prediction - WHO_COVID19$New_deaths
  RSS[model] = sum(err ^ 2) #R ^ 2 / r-squared
}


print_summary <- function(model, mean_mse, cv_std) {
  #prints summary for each model that is trained to manually inspect outputs
  print(model(summary))
  print(rmse)
  print(RSS)
  print(cv_std)
}


save_results <- function(model, mean_mse, predictions, feature_importances) {
  #saves model, model summary, feature importances and predictions list with test dataset 
}
 
 
if__name__ == "_main_":
  #define inputs
  "https://covid19.who.int/WHO-COVID-19-global-table-data.csv, https://covid19.who
  .int/table"


  #define variables 
  #categorical_vars = {"Country_code", "Country", "WHO_region", "Date_reported = 
  #                  wday", "Date_reported = month"}
  #numerical_vars = {"New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths"} 
  #target_var = "New_deaths predictions"



  #Load data - loads csv to (R equiv of pandas) dataframe
  load_file()
  print("loading data")
  WHO_COVID19 = load_file("https://covid19.who.int/WHO-COVID-19-global-table-data.csv")


  #clean datasets - get data ready for modeling, drop outliers seen in EDA 
  #(duplicates, Country_code)
  clean_data_noCountryCode()
  clean_data_nofullduplicates()
  clean_data_nopartialduplicates()
  
  
  #encode categorical variables and get final feature dfs
  get_wday_month()
  
  #get target/test dataframe - define test-train split
  calculate_test_train_split()
  
  #Initialise model list and dicts - to keep track of 
  models = {} #the number of models i've trained
  mean_mse = {} #use rmse here instead to relate back to how much ur predictions are off (how good they are 
  #'on avg, my predictions are within .... of the actual 'New deaths' values)
  cv_std = {} #to track standard deviation during cross validation
  res = {} #optional - track this to examine your errors
  
  
  #define number of processes to run in parallel
  num_procs = 2 #or 4, 8, 16. if have 4 core processor with hyperthreading, can 
  #run 8 simultaneously & potentially get a speed-up
  
  #shared model parameters
  verbose_lvl = 5 #will tell 'sidekit learn' to print out extra info when training 
  #the models. Helpful with R.Forest or Grad. Boosting to know no of trees trained
  #Also helps learn how far along in training process it is.
  
  
  #create models -- hyperparameter tuning already done by hand for each model
  lr = LinearRegression() #might be reasonable based on seeing some linear relat in EDA
  gam = GeneralisedAdditiveModel() #check what internal parameters shld be 
  rf = RandomForestRegressor(n_estimators = 60, n_jobs = num_procs, max_depth = 
       25, min_samples_split = 60, max_features = 30, verbose = verbose_lvl)
  #both gam and rf, start by tuning by hand and eventually figure out what's good 
  #max.depth & max_features (for rf, but don't tune n_estimators as more trees is
  #always better for prediction accuracy. Pick as big of a value as can reasonably 
  #handle. This depends on no of records i.e. datasets you're using). Use as big 
  #a value as you can go up to - and look at accuracy at each iteration. Once u
  #start getting marginal returns in the 'out of bag' score, you can stop 
  #increasing the no of trees.
  
  models.extend(lr, gam, rf) #adding the models being used to empty list above


  #parallel cross-validate models, using MSE as evaluation metric, and print summaries
  print("Beginning cross validation")
  for(model in models) {
    train_model(model, train_df, test_df, num_procs, rmse, cv_std)
    print_summary(model, rmse, cv_std)
  }
  #here, looping thru all the models defined, train it & print out summary  
  #here, used a function in a loop to train 5 different models and automatically 
  #select the best one - instead of doing these things manually. Slow, unreliable, 
    #risky as if manual, can't really test them. Need to be able to automate to test them. 
    
  #choose model with lowest rmse
  model = min(RMSE, key - rmse.get)
  #look up the smallest value and return the key associated with that value
  print('\nPredictions calculated using model with lowest rmse')
  print(model)
  
  #train model on entire training dataset
  model.fit(train_df)
  #note - during x-val, each iteration only used a subset of the records, now 
  #we'll train the model based on ALL of the training information
  
  #Create predictions based on test data
  predictions = model.predict(test_df)
  
  #store feature importances - already calculated, just saving here
  if(hasattr(model) == "feature_importances_") {
    print("importances = model.feature_importances_")
  } else {
    #checking if model attr has feature importances 'cos linear models don't have 
    #feature importances. so if it has feat import, we'll save this as 'importances'
    print("importances = (0)*len(feature_df.columns)")
    #'else' if it doesn't, create the list of zeros for feature importance to 
    #'#indicate that we don't know what those most import features necessarily are. 
  }

    feature_importances = pd.Dataframe9({'feature': feature_df.columns, 
      'importance': importances})
    feature_importances.sort_values(by = 'importance', ascending = FALSE, 
                                    inplace = TRUE)
    #next, taken all above and turned into a pandas dataframe & sorted all records 
    #in dataframe in descending order, to see which are the most important features,
    #how much of an impact on the model did they have relative to each other
    
    #print this out and save our results using the 'save_results' function
    save_results()