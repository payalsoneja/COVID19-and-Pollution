#install.packages("readxl")
install.packages("ggplot2")
install.packages("xts")
install.packages("ggfortify")
install.packages("ggthemes")
install.packages("grid")
install.packages("extrafont")
install.packages("dygraphs")
install.packages("purrr")
install.packages("lubridate") # for date functions
install.packages ("devtools")
install.packages("xlsx") 
install.packages("ropenaq") # to download air quality data from openaq
install.packages("plotly")
#
install.packages("ggmap")
install.packages("stringr")
install.packages("viridis")
install.packages("gganimate")

###### Load libraries ##########
#library(readxl)
library(ggplot2)
library(cowplot)
library(dygraphs)
library(xts) # To make the convertion data-frame / xts format
library(reshape2)
library(ggthemes)
library(grid)
library(extrafont)
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(xlsx)
library(ropenaq)
library(plotly)
##heat maps packages
library(ggmap)
library(stringr)
library(viridis)

# set the working directory
setwd("C:/Users/TCD/Documents/GitHub/covid_pollution")

####### Method 1: Using API #########

#countriesTable <- aq_countries()
#citiesTableIndia <- aq_cities(country="IN")

#aq_measurements(country = NULL, city = NULL, location = NULL,
#parameter = NULL, has_geo = NULL, date_from = NULL,
#date_to = NULL, limit = 10000, value_from = NULL,
#latitude = NULL, longitude = NULL, radius = NULL,
#attribution = FALSE, averaging_period = FALSE, source_name = FALSE,
#value_to = NULL, page = NULL)

source("ropenaq_measurement.R") #attaching the openaq measurement R file

# we have pollution data until the week of 25th April 2020
# The trouble with this is that we can't extract data greater than 2 or 3 months old. 
# They are stored in separate servers and we would need an amazon web server account to extract it.

start_dates <- seq(from = ymd('2020-04-25'), to = ymd(Sys.Date()), by='weeks') #lubridate

# pull data from openAQ server
for (i in 1:length(start_dates)) { 
print(start_dates[i])
  if (i != length(start_dates)) {
pollution_25 <- aq_measurements(country="IN", city="Delhi", parameter = c("pm25"), date_from = start_dates[i], date_to = start_dates[i+1])
pollution_10 <- aq_measurements(country="IN", city="Delhi", parameter = c("pm10"), date_from = start_dates[i], date_to = start_dates[i+1])
  }
  else {
pollution_25 <- aq_measurements(country="IN", city="Delhi", parameter = c("pm25"), date_from = start_dates[i])
pollution_10 <- aq_measurements(country="IN", city="Delhi", parameter = c("pm10"), date_from = start_dates[i])
}

write.csv(pollution_25, paste0("pm25_delhi_",start_dates[i],".csv")) ### save as csv files
write.csv(pollution_10, paste0("pm10_delhi_",start_dates[i],".csv"))
}

#load csv files
pollution_final <- do.call(rbind,
lapply(list.files(pattern="*.csv"), read.csv))

####### Data cleaning
#inspect
pollution_final %>% glimpse()

unique(pollution_final$dateLocal)

pollution_final <- pollution_final %>%
  arrange (location, dateLocal)

#count number of stations by date
pollution_final %>%
group_by(dateLocal) %>%
summarise(n= n_distinct(location)) %>%
print(n = 42)

n_distinct(pollution_final$location) # 36 unique locations
unique(pollution_final$location)  # list of 36 locations

# creating date variable
pollution_final$local_date <- date(pollution_final$dateLocal) # extracting dates from the local date variable
pollution_final <- pollution_final %>%
    arrange (location, local_date)


# create date and time variable to extract local date and time from dateUTC 
# and cut the data into 60 minute groups

pollution_final$local_time <- format(as.POSIXct(pollution_final$dateUTC,tz="GMT"), tz="", usetz=TRUE)
pollution_final$local_time_60 <- format(as.POSIXct(pollution_final$local_time), format='%Y-%m-%d %H:00:00')

# remove date (4th May) as we don't have all the data for all the stations
pollution_final <- pollution_final %>%
 filter (local_date < "2020-05-04")

pollution_final[rowSums(is.na(pollution_final)) > 0,] # check for NA values
rowMeans(is.na(pollution_final))  #  no NAs

#rename PM variables
pollution_final$parameter <- gsub("pm25", "PM2.5", pollution_final$parameter)
pollution_final$parameter <- gsub("pm10", "PM10", pollution_final$parameter)

#group by all and take city average # for date line plot
pollution_final_city <- pollution_final %>%
  group_by(city, local_date, parameter) %>%
  summarise(Mean=mean(value))   

#group by all and take city average # for hourly line plot
pollution_final_time <- pollution_final %>%
  group_by(city, local_time_60, parameter) %>%
  summarise(Mean=mean(value))  

## creating function for hourly line plot for all dates
hourly_plot <- function(pollution)
{hour_plotly <- ggplot(pollution, aes(x = as.POSIXct(local_time_60, tz=""), y = Mean, color = parameter)) 
hour_plotly <- hour_plotly + geom_line(size=0.5) +
  #scale_x_date(date_breaks = "1 day" , date_labels = "%b %d") +
  scale_y_continuous(limits = c(0,350), breaks=seq(0,350,50)) +
  geom_vline(xintercept=unclass(as.POSIXct("2020-03-22 00:00:00")),color="grey", linetype="dashed", size=0.6) +
  geom_vline(xintercept=unclass(as.POSIXct("2020-03-24 00:00:00")),color="grey", linetype="dashed", size=0.6) 
 # geom_vline(xintercept=unclass(as.POSIXct("2020-04-05 21:00:00")),color="grey", linetype="dashed", size=0.6) 

hour_plotly <- hour_plotly + labs(title = "Delhi Pollution levels before and after lockdown",
                                  x = "Date", y = "Particulate matter \n ug/m3\n ") + theme_economist() +
  scale_color_hue(labels = c("PM2.5", "PM10"))
hour_plotly <- hour_plotly + theme(
  legend.title=element_blank(),
  legend.text = element_text(size = 10),
  plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  axis.text.x = element_text(color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  text=element_text(family="Times"))

fig_time<- ggplotly(hour_plotly, dynamicTicks = TRUE) %>%
  layout(legend = list(orientation = "v", x = 1, y =0.5), 
         annotations = list(x = c(as.POSIXct("2020-03-22 00:00:00"),as.POSIXct("2020-03-24 00:00:00"), 1, as.POSIXct("2020-04-05 21:00:00")), y = c(370,370, -0.65, 370), 
                            text = c("Junta Curfew", "Complete Lockdown", "Data source: OpenAQ", "9PM 9minutes"), showarrow = F, textangle = c(90,0,0,90), 
                            xanchor = c("left", "left", "left", "left"), yanchor = "top",  xref= c("x", "x", "paper", "x"), 
                            yref = c("y", "y", "paper", "y"), font = list(size=14))) # set the position of vertical line and the position of legend

fig_time <- fig_time %>%
  rangeslider() %>%
  layout(hovermode = "x", xaxis=list(nticks=12, autorange=TRUE), yaxis=list(autorange=TRUE)) #show 12 hours on x axis and change Y axis accordingly as we scroll
return(fig_time)
}
fig_time <- hourly_plot(pollution_final_time)
fig_time
fig_time_April5 <- hourly_plot(pollution_final_time_April5)


## hourly line plot for all dates
hour_plotly <- ggplot(pollution_final_time, aes(x = as.POSIXct(local_time_60, tz=""), y = Mean, color = parameter)) 
hour_plotly <- hour_plotly + geom_line(size=0.5) +
  #scale_x_date(date_breaks = "1 day" , date_labels = "%b %d") +
  scale_y_continuous(limits = c(0,370), breaks=seq(0,370,50)) +
  geom_vline(xintercept=unclass(as.POSIXct("2020-03-22 00:00:00")),color="grey", linetype="dashed", size=0.8) +
  geom_vline(xintercept=unclass(as.POSIXct("2020-03-24 00:00:00")),color="grey", linetype="dashed", size=0.8) 
  #annotate(geom = "text", x=(as.POSIXct("2020-03-22 00:00:00")), y=350, label="Junta Curfew", size = 4, angle=90, vjust = 1, hjust=1) +
  #annotate(geom = "text", x=(as.POSIXct("2020-03-24 00:00:00")), y=350, label="Complete lockdown", size = 4, angle=90, vjust = 1, hjust=1)

hour_plotly <- hour_plotly + labs(title = "Delhi Pollution levels before and after lockdown",
                                  x = "Date", y = "Particulate matter \n ug/m3\n ") + theme_economist() +
  scale_color_hue(labels = c("PM2.5", "PM10"))
hour_plotly <- hour_plotly + theme(
  legend.title=element_blank(),
  legend.text = element_text(size = 10),
  plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  axis.text.x = element_text(color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  text=element_text(family="Times"))
#plot(hour_plotly)

fig_time<- ggplotly(hour_plotly, dynamicTicks = TRUE) %>%
  layout(legend = list(orientation = "v", x = 1, y =0.5), 
         annotations = list(x = c(as.POSIXct("2020-03-22 00:00:00"),as.POSIXct("2020-03-24 00:00:00"), 1), y = c(370,370, -0.65), 
                            text = c("Junta Curfew", "Complete Lockdown", "Data source: OpenAQ"), showarrow = F, textangle = c(90,0,0), 
                            xanchor = c("left", "left", "left"), yanchor = "top",  xref= c("x", "x", "paper"), 
                            yref = c("y", "y", "paper"), font = list(size=14))) # set the position of vertical line and the position of legend
fig_time <- fig_time %>%
  rangeslider() %>%
  layout(hovermode = "x", xaxis=list(nticks=10, autorange=TRUE)) #show 10 hours on x axis and change Y axis accordingly as we scroll
fig_time

#publish on plotly platform
Sys.setenv("plotly_username" = "sonejapayal")
Sys.setenv("plotly_api_key" = "a1c9xWtMULGSdfHi10Gy")
api_create(x = fig_time, filename = "Delhi_pollution_April12", fileopt = "overwrite",sharing = "public" ,username = "sonejapayal")


# filter data by time spamp date
pollution_final_time_April5 <- pollution_final_time %>%
  filter(local_time_60 >= "2020-04-05 00:00:00")

#line plot of April 5 
April5_plotly <- ggplot(pollution_final_time_April5, aes(x = as.POSIXct(local_time_60, tz=""), y = Mean, color = parameter)) 
April5_plotly <- April5_plotly + geom_line(size=0.8) +
  scale_x_datetime(date_breaks = "5 hours" , date_labels = "%b %d %H:%M") +
  scale_y_continuous(limits = c(0,200), breaks=seq(0,200,20)) +
  geom_vline(xintercept=unclass(as.POSIXct("2020-04-05 21:00:00")),color="grey", linetype="dashed", size=0.6) 


April5_plotly <- April5_plotly + labs(title = "Delhi Average Pollution levels on April 5\n before and after 9PM 9Minutes",
                                  x = "Date", y = "Particulate matter \n ug/m3\n ") + theme_economist() +
  scale_color_hue(labels = c("PM2.5", "PM10"))
April5_plotly <- April5_plotly + theme(
  legend.title=element_blank(),
  legend.text = element_text(size = 10),
  plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  axis.text.x = element_text(color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  text=element_text(family="Times"))

fig_April5 <- ggplotly(April5_plotly, dynamicTicks = TRUE) %>%
  layout(legend = list(orientation = "v", x = 1, y =0.5), #legend orientation
         annotations = list(x = c(-0.12), y = c(-0.55), 
                            text = c("Data source: OpenAQ"), showarrow = F, textangle = c(0), 
                            xanchor = c("left"), yanchor = "top",  xref= c("paper"), 
                            yref = c("paper"), font = list(size=14))) # set the position of vertical line and the position of legend

fig_April5 <- fig_April5 %>%
  rangeslider() %>%
  layout(hovermode = "x", xaxis=list(nticks=15), yaxis=list(autorange=TRUE)) #show 12 hours on x axis and change Y axis accordingly as we scroll
fig_April5

#publish on plotly platform
Sys.setenv("plotly_username" = "sonejapayal")
Sys.setenv("plotly_api_key" = "a1c9xWtMULGSdfHi10Gy")
api_create(x = fig_April12, filename = "Delhi_pollution_April12", fileopt = "overwrite",sharing = "public" ,username = "sonejapayal")



###################################
### Date Line plot for average pollution levels in Delhi #####

line_plot <- ggplot(pollution_final_city, aes(x = local_date, y = Mean, color = parameter)) 
line_plot <- line_plot + geom_line(size=1) +
  scale_x_date(date_breaks = "7 days" , date_labels = "%b %d") +
  scale_y_continuous(limits = c(0,350), breaks=seq(0,350,50)) +
  geom_vline(xintercept=unclass(as.Date("2020-03-22")),color="grey", linetype="dashed", size=0.8) +
  geom_vline(xintercept=unclass(as.Date("2020-03-24")),color="grey", linetype="dashed", size=0.8) +
  annotate(geom = "text", x=(as.Date("2020-03-22")), y=350, label="Junta Curfew", size = 4, angle=90, vjust = 1, hjust=1)+
  annotate(geom = "text", x=(as.Date("2020-03-24")), y=350, label="Complete lockdown", size = 4, angle=90, vjust = 1, hjust=1)

line_plot <- line_plot + labs(title = "Delhi Pollution levels before and after lockdown",
                              x = "Date", y = "Particulate matter \n ug/m3\n ",
                              caption = "Data: openaq.org") + theme_classic() +
                               scale_color_hue(labels = c("PM2.5", "PM10")) +
                              scale_color_manual(values=c('#F0E442','#0072B2'))
line_plot <- line_plot + theme(
  legend.title=element_blank(),
  legend.position="bottom",
  plot.title = element_text(color = "black", size = 12, face = "bold",  hjust = 0.5),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  axis.text.x = element_text(color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  text=element_text(family="Times"))
  plot(line_plot)
  
#plotly graph
line_plotly <- ggplot(pollution_final_city, aes(x = local_date, y = Mean, color = parameter)) 
line_plotly <- line_plotly + geom_line(size=0.5) +
  scale_x_date(date_breaks = "15 days" , date_labels = "%b %d") +
  scale_y_continuous(limits = c(0,350), breaks=seq(0,350,50)) +
  #geom_vline(xintercept=unclass(as.Date("2020-03-22")),color="black", size=0.2) +
  geom_vline(xintercept=unclass(as.Date("2020-03-24")),color="black", size=0.2) 
  
line_plotly <- line_plotly + labs(title = "Delhi Pollution levels before and after lockdown",
                              x = "Date", y = "Particulate matter \n ug/m3\n ") + theme_classic() +
                             scale_color_hue(labels = c("PM2.5", "PM10"))
line_plotly <- line_plotly + theme(
  legend.title=element_blank(),
  legend.text = element_text(size = 10),
  plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  axis.text.x = element_text(color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  text=element_text(family="Times"))

fig<- ggplotly(line_plotly, dynamicTicks = TRUE) %>%
  layout(legend = list(orientation = "v", x = 1, y =0.5), 
         annotations = list(x = c(as.Date("2020-03-22"),as.Date("2020-03-24"), 1), y = c(370,370, -0.6), 
         text = c("", "Complete Lockdown", "Data source: OpenAQ"), showarrow = F, textangle = c(90,0,0), 
         xanchor = c("left", "left", "left"), yanchor = "top",  xref= c("x", "x", "paper"), 
         yref = c("y", "y", "paper"), font = list(size=14))) # set the position of vertical line and the position of legend

fig <- fig %>%
  rangeslider() %>%
  layout(hovermode = "x", xaxis=list(nticks=15, autorange=TRUE)) #show 12 hours on x axis and change Y axis accordingly as we scroll
    
fig

#publish on plotly platform
Sys.setenv("plotly_username" = "sonejapayal")
Sys.setenv("plotly_api_key" = "a1c9xWtMULGSdfHi10Gy")
api_create(x = fig, filename = "Delhi_pollution_city_May5", fileopt = "overwrite",sharing = "public" ,username = "sonejapayal")

##################################

ggplot(pollution_final_city, aes(x = local_date, y = Mean, group = parameter)) + 
  geom_line() + 
  geom_segment(aes(xend = "2020-04-09", yend = Mean), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = Month), hjust = 0) + 
  transition_reveal(local_date) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))



#######################################
#filter to get for one date and one parameter
pollution_one_date <- pollution_final %>%
  filter(local_date == '2020-02-27', parameter == "pm25")
  
pollution_final <- pollution %>%
  group_by(location, city, local_date, parameter, latitude, longitude) %>%
  summarise(Mean=mean(value))   #group by all and take average of all stations in Delhi

#save as csv and excel files
write.csv(pollution_one_date, "one_date.csv")
write.xlsx(pollution_one_date, "one_date.xlsx")
##########################################


########### Heat maps ###################

# PLOT SCATTERPLOT
# - we'll do this as a quick data-check
ggplot() +
  geom_point(data = pollution_one_date, aes(x = longitude, y = latitude), alpha = .05)

# SIMPLE HEATMAP
ggplot() +
  stat_density2d(data = pollution_one_date, mapping=aes(x = longitude, y = latitude, fill = value), alpha=0.3, geom = "polygon") 

# GET MAP
register_google(key = "AIzaSyAdYQdXJeA7J2nxhSEWW70UAZm9Lptde0Q")
map_delhi <- get_map('Delhi', zoom = 10, maptype = "terrain-labels")
ggmap(map_delhi)

map <- get_googlemap('New Delhi', zoom = 11,
  maptype = "roadmap",
  style = c(feature = "all", element = "labels", visibility = "on")
)
ggmap(map)

# MAP WITH HEATMAP OVERLAY
ggmap(map) +
  stat_density2d(data = pollution_one_date, mapping=aes(x = longitude, y = latitude, fill = value), alpha=0.2, geom = "polygon", h =0.06) +
  scale_fill_viridis() +
labs(title = str_c('PM 2.5 concentrations on 27th February\n'
                                            ,'in Ug/m3'))+
       theme(text = element_text(color = "#444444")
             ,plot.title = element_text(size = 10, face = 'bold') 
       ,axis.text = element_blank()
     ,axis.title = element_blank()
     ,axis.ticks = element_blank()
     )

###################################################
### Creating Boxplot ###

### Group the data by different locations, parameters and local date 
pollution_location <- pollution_auto_final %>%
  group_by(local_date, location, parameter) %>%
  summarise(Mean=mean(value))   

### Filter data by pm2.5 and last week's date ###
pollution_pm25 <- pollution_location %>%
  filter(parameter == "pm25", local_date >= '2020-03-20') %>%
  filter (-Mean <= 0)

# Store the graph
box_plot1 <- ggplot(pollution_pm25, aes(x=as.factor(format(local_date, '%b %d')), y = Mean)) + geom_boxplot()
box_plot1 <- box_plot1 + scale_y_discrete(name = "PM2.5 in ug/m3") +
  scale_x_discrete(name = "Day") + ggtitle("PM2.5 concentrations in ug/m3") 
figure <- ggplotly(box_plot1)
figure
###################################

box_plot <- ggplot(pollution_pm25, aes(x=as.factor(format(local_date, '%b %d')), y = Mean))
box_plot <- box_plot + geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
  scale_y_discrete(name = "PM2.5 concentrations",
                     breaks = seq(0, 150, 50),
                     limits=c(0, 150)) +
  scale_x_discrete(name = "Day") +
  ggtitle("Boxplot of PM2.5 concentrations by day") +
  theme_economist() +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))
box_plot


  

 # mutate(N=n())

# #pollution_feb_pm25 <- read.csv("openaq_feb_pm25_feb_15_29.csv", na.strings = "")
# pollution_feb_pm10 <- read.csv("openaq_feb_pm10_feb_15_29.csv", na.strings = "")
# pollution_feb_pm25 <- read.csv("openaq_feb_pm25_mar_1_21.csv", na.strings = "")
# pollution_feb_pm25 <- read.csv("openaq_feb_pm10_mar_1_21.csv", na.strings = "")


