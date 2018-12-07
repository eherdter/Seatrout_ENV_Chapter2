# Seatrout_ENV_Chapter2

This repository holds scripts to:

1. Join all relevant environmental data downloaded from several different sources (EPA, USGS, Tampa Bay Water Atlas, Southwest Water Management District, St. Johns Water Management District) with the catch observations for young of the year spotted seatrout. 

* Data types include
  * Nutrient data
  * Rainfall data
  * Drought indices
  * Temperature data
  * Time Lagged Salinity Data
  * Time Lagged Temperature Data

2. Use helper functions for cleaning and filtering the environmental data
3. Use helper functions for joining the enviro data to the catch data using spatial and temporal rules
4. Use extreme gradient boosting (XGBoost) to determine the importance of each environmental predictor to young of the year recruitment. 
5. Use helpder functions for predictor screening and the functions of extreme gradient boosting. 
3. Visualize gain values (predictor importance). 


