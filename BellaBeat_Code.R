#' ---
#' title: "Bellabeat Analysis"
#' author: "Ryan T. Guertin"
#' date: "October 9, 2023"
#' ---

## BELLABEAT CODE ##

## Load Necessary Packages ##

library(tidyverse)
library(dplyr)
library(readxl)
library(cowplot)
library(gridExtra)
library(gganimate)
library(ggfortify)
library(knitr)
library(flexdashboard)
library(plotly)
library(rmarkdown)
library(tinytex)

## Load Data Sets 

bmi <- read.csv("bmi.csv")
daily_activity <- read.csv("daily_activity.csv")
heartrate <- read.csv("heartrate.csv")
intensities <- read.csv("hourlyintensities.csv")
sleep <- read.csv("sleep.csv")

## Cleaning Data: Col names and date/time issues already resolved in XL

## Checking each data set for null values

is.null(bmi)
is.null(daily_activity)
is.null(heartrate)
is.null(intensities)

## Checking each data set for na values

is.na(bmi)
#is.na(daily_activity)
#is.na(heartrate)
#is.na(intensities)
#is.na(sleep)

## daily_activity has three na columns that will be ommitted
## bmi has na values and we only consider the ID that have data

## Analyze intensities, determine fitness/activity level of users

unique(intensities$Id)

## Pulled the 33 unique Ids
## Get averages of each user, 1:33

user_33 <- intensities %>% 
  filter(Id == "8877689391")

avg_user_33 <- user_33 %>% 
  summarize(avg_intensity = mean(TotalIntensity))

as_tibble(avg_user_33)

## Create and Save Avg_Intensity Data Frame

Id <- c(1503960366, 1624580081, 1644430081, 1844505072, 1927972279, 2022484408, 2026352035, 2320127002, 2347167796, 2873212765, 3372868164, 3977333714, 4020332650, 4057192912, 4319703577, 4388161847, 4445114986, 4558609924, 4702921684, 5553957443, 5577150313, 6117666160, 6290855005, 6775888955, 6962181067, 7007744171, 7086361926, 8053475328, 8253242879, 8378563200, 8583815059, 8792009665, 887768939)

Intensity <- c(16.2, 8.1, 10.5, 5, 1.8, 17, 17, 8.7,
               14.5, 15.1, 15.4, 15.2, 4.36, 4.9, 11.3, 14.3,
               9.79, 14.4, 12.9, 12.8, 19.9, 12.5, 10.6, 4.37, 
               14.9, 17.6, 13.6, 17.9, 9.11, 14.9, 9.13, 4.43,
               19.1)

Avg_Intensity <- data.frame(Id, Intensity)
head(Avg_Intensity)

#Viz

Intensity_Plot <- ggplot(Avg_Intensity, aes(x =Id, y = Intensity, color = Intensity)) +
  geom_point(size=2.5) +
  ylim(0, 20.05)+
  geom_hline(yintercept = mean(Intensity), color="red") +
  geom_smooth() +
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey")) +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(title = "Average Intensity", subtitle = "By User", x = "User (Id)", y = "Intensity     Score")

Intensity_Plot
  
  
ggplotly(Intensity_Plot, tooltip = 'y') %>% 
  layout(title = list(text = paste0('Average Activity Intensity',
                                  '<br>',
                                  '<sup>',
                                  'By User', '</sup>')))

## Preparing to analyze activity by day of week. x=day, y=activity, color=time (pivotlong)

day_daily_activity <- separate(daily_activity, ActivityDate, into = c('Day', 'Date', 'Year'), sep = ',')

## Plot Activity reporting by day of week

Day_Activity <- day_daily_activity %>% 
  select(Day)

Daily_Plot <- ggplot(Day_Activity, aes(x = factor(Day, level = c('Sunday', 'Monday',          'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')), fill = Day)) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_manual(breaks=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday'   ,'Saturday'), values = c("Sunday" = "#0000ff", "Monday" = "#0000ff", "Tuesday" =         "#cc0099"   , "Wednesday" = "#cc0099", "Thursday" = "#cc0099", "Friday" = "#0000ff",     "Saturday" =  "#0000ff")) +
  labs(title="Activity Reporting", subtitle="By Day of Week", x="Day") +
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey")) 

Daily_Plot

ggplotly(Daily_Plot, tooltip = 'y') %>% 
  layout(title = list(text = paste0('Activity Reporting',
                                    '<br>',
                                    '<sup>',
                                    'By Day of Week', '</sup>')))


### Activity types and times per user ###

types_times <- daily_activity %>% 
  select(Id, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)
head(types_times)

p_types_times <- types_times %>% 
  pivot_longer(2:4, names_to = "Activity_Type", values_to = "Minutes")
head(p_types_times, n=12)

Activity_Level_User <- ggplot(p_types_times, aes(x = Id, y = Minutes, color     =Activity_Type,)) +
  geom_jitter(width = .5, size=1) +
  facet_wrap(~factor(Activity_Type, levels = c('LightlyActiveMinutes'  ,'FairlyActiveMinutes','VeryActiveMinutes'))) +
  scale_color_manual(breaks = c("LightlyActiveMinutes", "FairlyActiveMinutes"             ,"VeryActiveMinutes"), values = c("LightlyActiveMinutes"    = "#0000ff",                  "FairlyActiveMinutes" = "#cc0099", "VeryActiveMinutes" = "red")) +
  labs(title = "Activity Level by User", subtitle = "In Minutes", x = "User(Id)", color =   "Activity Level") +
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey")) 

Activity_Level_User

ggplotly(Activity_Level_User, tooltip = "y")


## Calories and Activity Level 

cal <- daily_activity %>% 
  select(Calories, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)
head(cal)

piv_cal <- cal %>% 
  pivot_longer(2:4, names_to = "Activity_Level", values_to = "Minutes")
head(piv_cal)

Calories <- ggplot(piv_cal, aes(x = Minutes, y = Calories, color = Activity_Level)) +
  geom_jitter(width = .5, size=.75) +
  facet_wrap(~factor(Activity_Level, levels = c('LightlyActiveMinutes'                      ,'FairlyActiveMinutes'   , 'VeryActiveMinutes'))) +
  scale_color_manual(breaks = c("LightlyActiveMinutes", "FairlyActiveMinutes"              ,"VeryActiveMinutes"), values = c("LightlyActiveMinutes"    = "#0000ff",                 "FairlyActiveMinutes" = "#cc0099", "VeryActiveMinutes" =   "red")) +
  labs(title = "Calories Burned", subtitle = "By Activity Level", color = "Activity Level")+
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey")) 

Calories

ggplotly(Calories, tooltip = "y")
  
##BMI and Activity Level

BMI <- merge(daily_activity, bmi) %>% 
  select(BMI, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes   ) %>% 
  arrange(BMI)
  head(BMI)

piv_BMI <- BMI %>% 
  pivot_longer(2:4, names_to = "Activity_Level", values_to = "Minutes")
  head(piv_BMI)

BMI_Activity <- ggplot(piv_BMI, aes(x = Minutes, y = BMI, color = Activity_Level)) +
  geom_jitter(width = .5, size=1) +
  geom_hline(yintercept = 24.9, color="black", size = 1.5) +
  geom_hline(yintercept = 30, color="black", size = 1.5) +
  scale_color_manual(breaks = c("SedentaryMinutes", "LightlyActiveMinutes",                   "FairlyActiveMinutes"), values = c("SedentaryMinutes" = "#0000ff", 
  "LightlyActiveMinutes" = "#cc0099", "FairlyActiveMinutes" = "red")) +
  labs(title = "Activity Level and BMI", color = "Activity Level") +
  annotate("text", x=1150, y=29, label="BMI over 24.9 is Overweight (CDC)", color="black" , 
  size=3.5, fontface = "bold") +
  annotate("text", x=1077, y=32, label="BMI over 30 is Obese (CDC)", color="black", 
  size=3.5, fontface = "bold") +
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey")) 

BMI_Activity

ggplotly(BMI_Activity, tooltip = "y")


## Activity Intensity by hour

  ## Create level order for ActivityHour

level_order <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")

# Plot

Hrly_Activity <- ggplot(intensities, aes(x = factor(ActivityHour, levels = level_order),   y = AverageIntensity, fill = AverageIntensity)) +
  geom_col(position = "dodge") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Intensity of Activity", subtitle = "By Hour", x = "Time", y = 
  "Avg Intensity", fill = "Intensity Level" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.background = element_rect("lightgrey")) +
  theme(panel.background = element_rect("darkgrey"))

Hrly_Activity

ggplotly(Hrly_Activity, tooltip = "y")
 
##Test Grid for "Fit"

plot_grid(Daily_Plot, Intensity_Plot, Activity_Level_User, Calories, BMI_Activity, Hrly_Activity) 



  
  
    



