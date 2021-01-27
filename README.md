## COVID-19 PREDICTION PROJECT - README

## INTRODUCTION
For my COVID-19 project, i chose to examine WHO datasets for patient morbidity and mortialities from COVID-19. I built and tested several predictive models and subsequently made comparisons between the predictions of the different models. Finally, a comparison was made between lockdown and social distancing approaches to the pandemic using the model with the best performance. 

Within months, COVID-19 went from an epidemic to a pandemic. From the first identified case in Wuhan, China in December 2019, to a declaration of a pandemic by the World Health Organisation in March 11, 2020. 

Coronavirus disease 2019 (COVID-19) is a contagious airborne disease caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). It is spread through respiratory droplets - through speaking, sneezing and coughing. 

Numerous different symptoms of COVID-19 have been documented but the most common are shortness of breath, a dry cough, fever, fatigue and loss of smell & taste.

Symptoms onset occur between 1 - 14 days after exposure to the virus. Most patients have mild symptoms but a few progress to develop acute respiratory distress syndrome (ARDS). ARDS can be precipitated by a cytokine storm, multi-organ failure, septic shock and blood clots.

People remain infectious for up to 10 days in moderate cases and much longer in more severe cases. The standard testing method is through use of real-time reverse transcription polymerase chain reaction (rRT-PCR) from a nasopharyngeal swab. 

Preventive measures include physical or social distancing, quarantining, hand washing, ventilation of indoor spaces and keeping unwashed hands away from the face. The use of face masks is mandatory for all (except young children and those that are medically exempt) in public settings to minimise transmission risk. Several different vaccines have been developed and are in use across various countries in the developed world.


## DATA COLLECTION
All the data i used for this project came from the WHO website. The WHO has collated data across all countries from the 3rd of January 2020, so that governments can monitor and learn from the pandemic. Datasets used for this project cover the period between 3rd of January and 5th of November 2020. 

Collected dataset features include: Date reported, Country, WHO Region, New Cases (reported on the day), Cumulative cases (to date each day), New deaths (reported on the day), Cumulative deaths (to date each day). 

There were 71675 datasets in total, each containing the above features.


## PRE-PROCESSING
As part of data cleansing, i implemented the following steps:

- Analysed the datasets for presence of full duplicates, of which there were none
- Analysed the datasets for the presence of partial duplicates, none were found
- Analysed the datasets for the presence of missing values. No true missing values were computed
- Split "Date_reported" column into weekday and month columns for further analysis and model inputs
- Country_code was removed as not found to be relevant to the analysis of the datasets


## EXPLORATORY DATA ANALYSIS
Before building my predictive models, i undertook exploratory data analysis to ascertain if any patterns were observable in the datasets:

- Of the 10 countries with the highest number of total deaths, there were 4 European countries (France, Italy, Spain and the United Kingdom) and 4 European countries in the top 10 group of highest cases (France, Spain)

| Country  |   total_cases |
| -------  | ------------  |
|Argentina |   1166924     |     
|Brazil    |   5535605     |     
|Colombia  |   1074184     |
|France    |   1381098     |
|India     |   8229313     |
|Mexico    |    924962   |
|Russia  |    1655034  |
|Spain   |    1185678  |
|U.K.  |      1034918  |
|U.S.A   |    9032465  |

- Visualised COVID-19 caseload and mortality rate in 3 European countries  chosen specifically for the differences in their policy approach to management of the pandemic (Germany, Sweden and the UK).

I subsequently focused in on the 3 European countries with differing policies wrt pandemic handling: - Germany (lockdown at an early staage with subsequent social distancing), Sweden (social distancing with no lockdown), and the UK (delayed lockdown with subsequent social distancing).

Graph plots showed equivalent peaks in new cases in Germany (total population approximately 83 million) and the UK (total population approximately 66 million), with approximately 6000 new cases during the first wave of the pandemic in each country. That in Sweden (total population approximately 10 million) was around 2000. This suggests the morbidity rate was highest in Sweden (0.002) and lowest in Germany (0.00072). This was observed between the months of March and April 2020. 

A different picture was seen with the mortality rates from COVID-19 during the first wave (in April - with a lag time of a few weeks post infection/becoming a New Case). This showed the UK as having the highest mortality rate with approximately 1250 new deaths (1.8 *10^5), closely followed by Sweden with 125 new deaths (1.25 *10^5) and finally, Germany with the lowest mortality rate at just 300 new deaths (3.6 * 10^5). 

The difference in morbidity and mortality rates between Germany and the UK due to COVID-19 could be attributable to a number of different reasons - Germany took action sooner than the UK to stop the spread of COVID-19, with early implementation of lockdown policies (inclusive of school & business closures, banning gatherings of people and mandating isolation of people who either had the virus or were exposed to it). There has also been a high level of testing across the wider population from the start (inclusive of milder cases in younger people) which permitted greater control over the ability of the virus to spread unchecked.

A boxplot of new cases by country revealed a greater inter-quartile range (and therefore a much bigger spread of the new cases data) in the UK compared to Germany and Sweden (mainly due to the location of the 3rd quartile in the UK). This was calculated to be 3335 cases in the UK.

In addition to the greater variability in the UK new cases data compared with the other 2 countries, larger outliers were also observed within the UK datasets (extending beyond the maximim and minimum values denoted by the whiskers in each boxplot). These outliers were calculated as values greater than 1.5 * IQR = 1.5 * 3335 = 5002.5 new cases.

This is seen in the graph plot from September onwards (the start of the second wave of the pandemic), with daily counts of new cases greatly exceeding the threshold of 5002 new cases and therefore denoted as outliers.


## MODELING
In line with the question to be addressed in this project, the datasets were split according to Country. As a result, 305 datasets were each used to test and train models that would predict Cumulative deaths (within the UK and Sweden). 

Based on the features available within the datasets and the EDA above, i chose the following features as my predictors:

- Cumulative Cases
- Month 
- Weekday

I chose a 2/3:1/3 train - test split for the models

The RMSE results of the regression models are shown below

Modeltype   rmse
pred.gam    371.0353
pred.lin    2554.9649
pred.rf     1281.9869
pred.xgb    260.2072

XGBoost clearly provides the best results with the lowest RMSE results so i used the algorithm to predict cumulative mortality rates in Sweden as well.