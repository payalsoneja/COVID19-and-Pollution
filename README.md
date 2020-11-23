# COVID 19 and Air Pollution - Data Visualization
A data-driven exploration of air pollution levels during different stages of restrictions imposed during COVID-19
<p align="center">
  <img src="https://raw.githubusercontent.com/payalsoneja/COVID19-and-Pollution/master/Output/plot_April5.gif" />
</p>


### 1. Extracting data from OpenAQ using R
+ #### Setting up `ropenaq`:
OpenAQ has an R package [ropenaq](https://cran.r-project.org/web/packages/ropenaq/index.html) that gives easy access to their real time data [API](https://docs.openaq.org/). The first step is to use the function `aq_measurements` to fetch this data based on the requirements. For the scope of this use case, the focus is on Delhi, India. 

+ #### Weekly batches:
`date_from` and `date_to` parameters control the temporal scope of extracted data. The code when run extracts data in chunks of weeks, until the current date. A csv file is saved to working directly for each week's extract.

### 2. Cleaning and Transforming the data for visualization
`covid_pollution.R` performs the following steps in order:
+ Loads and appends all extracted csv files into a dataframe
+ Sorts the dataframe by `location` and `dateLocal`
+ Converts `dateLocal` to R date variable
+ Creates time variable from `dateUTC` and cuts it into 60 minute intervals
+ Handles exceptions with dates missing data
+ Groups the data two ways:
   + By `city`, `date`, `parameter` to plot average daily levels 
   + By `city`, `hour`, `parameter` to plot average hourly levels 

### 3. Plotting
+ `Hourly` and `daily` plots help analyze on macro and micro level how pollution levels are going down in Delhi with a halt to day-to-day activities. 
+ Veritcal line segments separate time periods of different restrictions imposed by government
+ line plots are generated using `ggplot` and then converted to `plotly` compatible plots using `ggplotly` package
+ `rangeslider` provides functionality to zoom the date/time axis in and out 
+ Publishing to `plotly` requires setup that will generate username and api key 
