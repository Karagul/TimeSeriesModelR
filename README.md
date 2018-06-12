# TimeSeriesModelR
Shiny Web App for Modeling and Forecasting with SARIMA Models


In the fall of 2017 I took a class in time series analysis. Due to its lack of interactive modleing and forecasting features, it was highly suggested we avoid using R for our projects. I found this a bit frustrating because it meant I had to do all my work inside one of the university's computer labs instead of being able to work from home. Seeing a need, I began working on TimeSeriesModelR, an online application for building SARIMA models, and using them to make forecasts. The application is built entirely using the R Shiny package.

Currently, the application allows for the uploading of data through .rds files, plots the data, suggests a lambda values for Box-COx transformations, allows for first and seasonal differencing, and provides ACF and PAC plots. Users can also divide the data into a training set, build SARIMA models, and check the validity of these models using residual plots (including ACF and PACF plots), Box-Pierce tets, and hypothesis tests for model paramter values. Forecasting capabilities will  be added in the very near future, and there are also plans to add capabilities for more transforamtion options and inervention analysis.

I have included two data sets for anyone who would like test the current version of TimeSeriesModelR. One of the data sets is the famous Sunspots data from 1700 to 1988. The other is the dataset concerning the water levels at Lake Huron from 1875 to 1972.

Please not, when transforming a variable, you MUST supply a name for the new variable.
