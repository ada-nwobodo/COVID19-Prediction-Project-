#__author__ = 'Dr Adaeze Nwobodo'
#__email__ = 'AdaezeNwobodo@gmail.com'

#A) IMPORT PACKAGES
#import library packages {caret/Dplyr/lubridate/mgcv/purrr/ranger/rpart/
#rpart.plot/sigr/tidyselect/tidyverse/wrapr/base/broom/cluster/datasets/ellipsis
#/ggplot2/graphics/grDevices/highr/lattice/maggritr/methods/nlme/readr/readxl/
#renv/stats/stringr/tibble/tidyr/utils}

#B) LOAD THE DATA
load_file()

#C) EXAMINE THE DATA
head(WHO_COVID19)

#C-1) DATA CLEANING
#Check for full duplicates***FUNCTION
full_duplicates <- function(dataframe) {
  return (sum(duplicated(dataframe)))
}

full_duplicates(WHO_COVID19)
#function confirms there are no full duplicates within the datasets

#Check for partial duplicates***FUNCTION***
partial_duplicates <- function(dataframe) {
  return  (dataframe %>%
          count(Country) %>%
          filter(n > 1))
}

WHO_partial_duplicates <- partial_duplicates(WHO_COVID19)
# this confirms there are 235 partial duplicates in the datasets (of n = 305)
WHO_partial_duplicates

table(WHO_partial_duplicates)
#this confirms the duplicates are not duplicated data, but merely unique records 
#of 1 for each country.

#Check for uniformity issues (NOT BEEN ADDED - WILL USE BOXPLOT FOR ***OUTLIERS***)
WHO_COVID19_GUS_DATES %>%
  ggplot(aes(month, New_deaths)) + geom_point()
#outliers are a sign of uniformity issues so do basic plotting to identify any outliers

#Completeness and missing data - finding missing values
is.na(WHO_COVID19)

#for total number of missing values ***FUNCTION***
missing_data <- function(filename) {
  sum(is.na(filename))
}

sum(is.na(WHO_COVID19))

#drop missing values****FUNCTION NOT WORKING****
drop_missing_data <- function(filename) {
  filter(is.na(filename))
}

filter(is.na(WHO_COVID19))

WHO_no_missing_data <- drop_missing_data(WHO_COVID19)


#C-2 - EXPLORATORY DATA ANALYSIS
#summarise dataset variables 
#categorical_vars = {"Country_code", "Country", "WHO_region", "Date_reported = 
#                  wday", "Date_reported = month"}
#numerical_vars = {"New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths"} 
#target_var = "New_deaths predictions"

#Drop Country Code from dataset - not relevant to the project 
#Select columns to remove Country Code

WHO_COVID19 %>%
  select(-Country_code)

#Filter WHO Region to focus on EUROPE
WHO_COVID19_EURO <- WHO_COVID19 %>%
  filter(WHO_region == "EMRO", "WPRO", "AFRO", "AMRO", "SEARO")

#Visualising plot with faceting(by country)
WHO_COVID19_EURO %>%
  ggplot(aes(x=Date_reported, y=Cumulative_cases)) + geom_line() + facet_wrap(~Country) +
  ggtitle("Cumulative Distribution of covid cases by EU country Jan - Nov 2020")
#Visible spikes showing first and second waves in UK, France, Italy, Spain etc

# Top 5 EURO countries hardest hit (Cumulative Cases) by pandemic by early Nov 2020
# Groupby Country and summarise total Cumulative EURO cases
Top5Cases_WHO_COVID19_EURO <- WHO_COVID19_EURO %>%
  group_by(Country) %>%
  summarise(total_cases = max(Cumulative_cases)) %>%
  top_n(5, total_cases)


# see the result
Top5Cases_WHO_COVID19_EURO
#France, Italy, Russian Federation, Spain, The United Kingdom

#Plot of Top 5 cases in Europe 
WHO_COVID19_EURO %>%
  ggplot(aes(Date_reported, Cumulative_cases, group = Top5Cases_WHO_COVID19_EURO)) + geom_line() + 
  ylab("Cumulative confirmed cases top 5 EU countries")

#Top 5 EURO contries with highest number of deaths by early Nov 2020
Top5Deaths_WHO_COVID19_EURO <- WHO_COVID19_EURO %>%
  group_by(Country) %>%
  summarise(total_deaths = max(Cumulative_deaths)) %>%
  top_n(5, total_deaths)

# see the result
Top5Deaths_WHO_COVID19_EURO
#Same countries as above for Top 5 Euro countries with cumulative cases by Nov 2020

#Filter EURO datasets for Germany, UK and Sweden only
WHO_COVID19_EURO_GUS <- WHO_COVID19_EURO %>%
  filter(Country == "Germany" | Country == "The United Kingdom" | Country == "Sweden")

#***AT THIS STAGE, SPLIT "DATE-REPORTED" COLUMN INTO WDAY AND MONTH COLUMNS*****

#Visualise plots of Cumulative Cases in Germany, UK and Sweden (with facets)
WHO_COVID19_EURO_GUS %>%
  ggplot(aes(Date_reported, Cumulative_cases)) + geom_line() + 
  facet_wrap(~Country) + ggtitle("Distribution of cumulative Covid cases in 
  Germany, UK and Sweden Jan - Nov 2020")
#shows exponential rise in case number between end of March and May, with 
#levelling off in June and a second exponential rise from September for all 3

#USE LOG SCALE FOR A BETTER FIT TO THE DATA
#Log-scale of cumulative confirmed cases for GUS
WHO_COVID19_EURO_GUS %>%
  ggplot(aes(Date_reported, Cumulative_cases)) + geom_line() + geom_smooth(method = "lm",
  se = FALSE) + facet_wrap(~Country) + ylab("Cumulative confirmed cases") + scale_y_log10()
 
#log-scale of cumulative confirmed deaths for GUS
WHO_COVID19_EURO_GUS %>%
  ggplot(aes(Date_reported, Cumulative_deaths)) + geom_line() + geom_smooth(method = "lm",
  se = FALSE) + facet_wrap(~Country) + ylab("Cumulative confirmed deaths") + scale_y_log10()
#with the log scale - not getting a closer fit to the data, but curve is more linear
#this suggests that the COVID-19 cases in the 3 countries are growing at an exponential rate
#albeit not all 3 countries being equally affected.
#might possibly be more of a challenge from a data science pov - wrt choice of model

#Visualise plots of new covid Deaths in Germany, UK and Sweden (with facets)
WHO_COVID19_EURO_GUS %>%
  ggplot(aes(Date_reported, New_deaths)) + geom_line() + facet_wrap(~Country) +
  ggtitle("Distribution of new Covid deaths in Germany, UK and Sweden Jan - Nov
  2020")

#Boxplot of Cumulative Cases of COVID in GUS countries
WHO_COVID19_EURO_GUS %>%
  ggplot(aes(Country, Cumulative_cases)) + geom_boxplot() +
  ggtitle("Boxplots of Cumulative cases - no log scale")


#Boxplot of Cumulative cases(log scale) of COVID in GUS countries
WHO_COVID19_EURO_GUS %>%
  mutate(log_cum_cases = log(Cumulative_cases)) %>%
  ggplot(aes(Country, log_cum_cases)) + geom_boxplot() + 
  ggtitle("Boxplots of Cumulative COVID cases in GUS - logscale")



#Create boxplot of New Cases (log scale) of COVID in GUS countries
WHO_COVID19_EURO_GUS %>%
  mutate(log_new_cases = log(New_cases)) %>%
  ggplot(aes(x = Country, y = log_new_cases)) + geom_boxplot() + 
  ggtitle("Boxplots of New COVID cases in Germany, UK and Sweden - logscale")
#INTERPRETATION - this shows that the medium length of new cases is highest in
#the UK followed by Germany then Sweden but there is still a reasonable amount 
# of overlap between the numbers of new cases in all 3 countries. 
#INTERPRETATION - log function of new cases taken as the curves all have heavy 
#right skew. this drew in right tail and spread out lower values so we better 
#interpret the results. Sweden records the highest density & UK (bimodal) distri

#Create boxplot of New Deaths (log scale) of covid in GUS countries
WHO_COVID19_EURO_GUS %>%
  mutate(log_new_deaths = log(New_deaths)) %>%
  ggplot(aes(x = Country, y = log_new_deaths)) + geom_boxplot() + 
  ggtitle("Boxplots of New COVID deaths in Germany, UK and Sweden - logscale")
#INTERPRETATION - as above, highly skewed distributions can make it very diffi
#cult to learn anything from a vis. Transformations (using log scale) can be hel
#pful in revealing the more subtle structure.

#Filter GUS datasets for UK datasets alone

WHO_COVID19_UK <- WHO_COVID19_EURO_GUS %>%
  filter(Country == "The United Kingdom")


#Filter GUS datasets for Swedish datasets alone 

WHO_COVID19_Sweden <- WHO_COVID19_EURO_GUS %>%
  filter(Country == "Sweden")

#Visualise target variable (New Deaths) for UK/ i.e. UK mortality rate 
WHO_COVID19_UK %>%
  ggplot(aes(x=New_deaths)) + geom_boxplot() + coord_flip() + ggtitle("New Deaths datasets for UK")
#gives an indication of range of predictions to expect from algorithms at the end

WHO_COVID19_Sweden %>%
  ggplot(aes(New_deaths)) + geom_boxplot() + coord_flip() + ggtitle("New Deaths datasets for Sweden")

#Identification of potential outliers using IQR for UK New Deaths 

third_quartile_UK <- quantile(WHO_COVID19_UK$New_deaths, 0.75)
third_quartile #164

first_quartile_UK <- quantile(WHO_COVID19_UK$New_deaths, 0.25)
first_quartile #2

interquartile_range_UK <- third_quartile_UK - first_quartile_UK
interquartile_range #162. This is corroborated visually by points above boxplot

#OR alternatively, interquartile_range <- IQR(filename)

#Id of potential outliers in UK New Deaths (1.5 * IQR above and below = 108 - 243)



#Turn IQR calculation into a ******function*****
third_quartile <- function(filename) {
  quantile(filename, 0.75)
}

first_quartile <- function(filename) {
  quantile(filename, 0.25)
}

interquartile_range <- third_quartile - first_quartile

#Identification of potential outliers using IQR for Sweden New Deaths 

third_quartile_Sweden <- third_quartile(WHO_COVID19_Sweden$New_deaths)
first_quartile_Sweden <- first_quartile(WHO_COVID19_Sweden$New_deaths)
interquartile_range_Sweden <- third_quartile_Sweden - first_quartile_Sweden
interquartile_range_Sweden #31. This is corroborated visually by the points above boxplot

#Examine and explain the meaning of the outliers you have detected for UK & Sweden
interquartile_range_UK <- 162

if(WHO_COVID19_UK$New_deaths > interquartile_range_UK * 1.5) {
  print("This is an outlier!")
} else if(WHO_COVID19_UK$New_deaths < interquartile_range_UK / 1.5) {
  print("This is also an outlier!")
} else {
  print("No problem here")
}

#Using coord_trans() to transform the coordinates of the plot

WHO_COVID19_UK %>%
  ggplot(aes(Cumulative_cases, Cumulative_deaths)) + geom_point() + 
  scale_x_log10() + scale_y_log10()


#Computing same graph for New cases vs new deaths
WHO_COVID19_UK %>%
  ggplot(aes(New_cases, New_deaths)) + geom_point() + scale_x_log10() + scale_y_log10() +
  ggtitle("Plot log-scale of new cases vs new deaths for UK")

#IS THIS the sort of code i need to further examine the outliers for UK New deaths
#How can i put the code above into an "is_outlier" variable? (See notes for further)


WHO_COVID19_UK_outlier <- WHO_COVID19_UK %>%
  mutate(is_outlier = New_deaths > 243 | New_deaths < 108) %>%
  filter(is_outlier) %>%
  arrange(desc(is_outlier))

WHO_COVID19_UK_outlier
head(WHO_COVID19_UK_outlier)

WHO_COVID19_UK_outlier <- WHO_COVID19_UK_outlier %>%
  select(-Country_code | -WHO_region)

WHO_COVID19_UK_outlier
#How do i visualise the 'is_outlier' column. Output shows original columns.....
#"with 266 more rows, and 2 variables: is_outlier <lgl>, Country_code <chr>
#How do i actually see the 'is_outlier' column?

#Repeat again for Sweden datasets

