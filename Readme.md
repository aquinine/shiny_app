## Information

Doing the basic exploratory analysis for most datasets is repetitive work that can be partially automated.  In this Shiny application, the user can load many of the MASS datasets and perform basic exploratory analysis.  The exploratory analysis provides basic information about the dataset:

*  Dimensions,
*  Names of the columns,
*  Example of the data, and
*  Pairs plot

The user can also chose one of the variables and provide preliminary prediction data.  When the user chooses a possible outcome, the application provides p-values and plots that show how well all of the other variables fit the outcome.  The user can also choose how many variables to combine based on a set number.  The application will provide a linear fit of the combined variables for the smallest p-values. 

With this application, the user can step through any of the possible outcomes to determine which one is the best variable to use.  A knitr version of the application's analysis can be downloaded so that it can be used later to jump start a report on the dataset.

## Instructions for Using the Application

To complete exploratory analysis the user chooses the data set from the drop down list at the top labeled "Chose a dataset."  Note that not all of the MASS datasets are listed.  Some of the datasets do not lend themselves to the prediction analysis and were eliminated from the list.  Once the dataset is chosen, the exploratory analysis is completed.  After choosing the dataset, the user can control the number of values shown in the example of data by changing the value in "numbers of observations to view."  

To trigger the preliminary prediction analysis, an outcome needs to be chosen from the drop down box labeled "Chose an outcome."  By default, the application chooses the number 2 for the number of predictors to combine to do a combined linear fit using multiple variables.  Note that this number can be changed to whatever the value the user wants.  If that number exceeds the number of variables, no combined prediction analysis will be completed.

If the user is interested in downloading a knitr script with the output from these analysis, clicking on the "Download knitr output" will download the knitr script into the download directory.  This script can be used in RStudio to create a knitr report with the same analysis in the application. 