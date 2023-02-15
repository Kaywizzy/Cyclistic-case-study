## Case Study: How Does a Bike-Share Navigate Speedy Success?

The purpose of this document is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

### Introduction
This exploratory analysis case study is towards Capstome project requirement for [Google Data Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics). The case study involves a bikeshare company's data of its customer's trip details over a 12 month period (January 2022 - December 2022). The data has been made available by Motivate International Inc. under this [license](https://www.divvybikes.com/data-license-agreement).

The analysis will follow the 6 phases of the Data Analysis process: Ask, Prepare, Process, Analyze, and Act. A brief explanation of these processes:

#### Ask

- Ask effective questions
- Define the scope of the analysis
- Define what success looks like
 
#### Prepare

- Verify data’s integrity
- Check data credibility and reliability
- Check data types
- Merge datasets

#### Process

- Clean, Remove and Transform data
- Document cleaning processes and results

#### Analyze

- Identify patterns
- Draw conclusions
- Make predictions

#### Share

- Create effective visuals
- Create a story for data
- Share insights to stakeholders

#### Act

- Give recommendations based on insights
- Solve problems
- Create something new

<br/>

### 1. Ask


#### Scenario

Marketing team needs to design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ.

#### Stakeholders:

- Director of marketing
- Cyclistic executive team

##### Objective

Hence, the objective for this analysis is to throw some light on how the two types of customers: annual members and casual riders, use Cyclistic bikeshare differently, based on few parameters that can be calculated/ obtained from existing data.

#### Deliverables:

- Insights on how annual members and casual riders use Cyclistic bikes differently
- Provide effective visuals and relevant data to support insights
- Use insights to give three recommendations to convert casual riders to member riders

<br/>

### 2. Prepare

#### Data Source

A total of **12 CSV files** have been made available for each month starting from **January 2022 to December 2022**. Each file captures the details of every ride logged by the customers of Cyclistic. This data that has been made publicly available has been scrubbed to omit rider's personal information.

The combined size of all the 12 CSV files is close to 1GB. Data cleaning in spreadsheets will be time-consuming and slow compared to R. I am choosing R simply because I could do both data wrangling and analysis/ visualizations in the same platform. 

<br/>

#### Load Libraries

```{r}
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)
```

<br/>

#### Load dataset CSV files (Previous 12 months of Cyclistic trip data)
```{r}
Trips_Jan22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202201-divvy-tripdata.csv")
Trips_Feb22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202202-divvy-tripdata.csv")
Trips_Mar22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202203-divvy-tripdata.csv")
Trips_Apr22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202204-divvy-tripdata.csv")
Trips_May22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202205-divvy-tripdata.csv")
Trips_Jun22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202206-divvy-tripdata.csv")
Trips_Jul22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202207-divvy-tripdata.csv")
Trips_Aug22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202208-divvy-tripdata.csv")
Trips_Sep22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202209-divvy-publictripdata.csv")
Trips_Oct22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202210-divvy-tripdata.csv")
Trips_Nov22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202211-divvy-tripdata.csv")
Trips_Dec22 <- read_csv("CAPSTONE PROJECT/CASE STUDY 1 - CYCLISTIC/202212-divvy-tripdata.csv")
```


### 3. Process

Combine all the datasets into one single dataframe

```{r}
all_trips <- bind_rows(Trips_Jan22, Trips_Feb22,Trips_Mar22, Trips_Apr22, Trips_May22, Trips_Jun22, Trips_Jul22,
                       Trips_Aug22, Trips_Sep22, Trips_Oct22, Trips_Nov22, Trips_Dec22)
str(all_trips)
```


<br/>

#### Remove unused columns not required or beyond the scope of project

```{r}
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

<br/>

#### Rename columns for better readability
```{r}
all_trips <- all_trips %>% rename(trip_id= ride_id,ride_type= rideable_type,start_time = started_at,
                                  end_time =ended_at,from_station_name = start_station_name
                                  ,from_station_id = start_station_id ,to_station_name = end_station_name
                                  ,to_station_id = end_station_id,usertype = member_casual)
glimpse(all_trips)
```


<br/>

```{r}
# Aggregate ride data for each month, day, or year
all_trips$date <- as.Date(all_trips$start_time) #The default format is yyyymm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips_v2$date), "%A")
```


```{r}
# Add a “ride_length” calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)
```


```{r}
# Add a “ride_length” calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)

```


```{r}
# Convert “ride_length” from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

```


```{r}
# check the dataframe
glimpse(all_trips)
```

<br/>

Let's remove the ride_length column has any negative values, as this may cause problem while creating visualizations. Also, we do not want to include the trips that were part of quality tests by the company. 


```{r}
# Remove “bad” data
skim(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
skim(all_trips_v2)
```


It is important to make sure that columns have distinct values. Let's confirm the same.

```{r}
# Clean columns names and removed duplicates
all_trips_v2 <- all_trips_v2 %>% 
  clean_names() %>% 
  unique()
```

<br/>

### 4&5. Analyze and Share Data

The dataframe is now ready for descriptive analysis that will help us uncover some insights on how the casual riders and members use Cyclistic rideshare differently.

First, let's try to get some simple statistics on ride_length for all customers, and do the same by user_type.

```{r}
# statistical summary of ride_length for all trips
summary(all_trips_v2$ride_length)
```

```{r}
#Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)
```

The mean ride length of member riders is lower than the mean ride length of all trips, while it is exactly the opposite for casual riders, whose mean ride length is higher than the the mean ride length of all trips. This tells us that casual riders usually take the bikes out for a longer duration compared to members.

<br/>

#### Total number of trips by customer type and day of the week
```{r}
# fix the order for the day_of_the_week variable so that they show up in the same sequence in output tables and visualizations

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日"))

#Total number of trips by user type and day of the week
all_trips_v2 %>% 
  group_by(usertype, day_of_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(ride_length)) %>% 
  arrange(usertype, desc(number_of_rides))
```


#### Visualization
```{r}
ggplot(all_trips_v2, aes(usertype, fill=usertype)) + 
  geom_bar() + 
  labs(x="Casuals vs Members", title="Casuals vs Members Distribution")
```
As we can see on the member x casual table, members have a bigger proportion of the dataset, composing ~59%, ~19% bigger than the count of casual riders.

```{r}
#Total trips by user type Vs. Day_of_Week
all_trips_v2 %>%  
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by user type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

From the table and graph above, casual customers are most busy on Sundays followed by Saturdays, while members are most busy on earlier half of the week extending to thursday. Interesting pattern to note though is the consistent trip numbers among members with less spread over entire week as compared to casual riders who don't seem to use the bikeshare services much during weekdays.

<br/>

#### Average number of trips by user type and month
```{r}
all_trips_v2 %>% 
  group_by(usertype, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(usertype,desc(number_of_rides))
```


#### Visualization
```{r}
#Total trips by user type Vs. Month
all_trips_v2 %>%  
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by user type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

The data shows that the months of May, June, July, August, and September are the most busy time of the year among both members and casual riders. This could be attributed to an external factor (eg. cold weather, major quality issue) that might have hindered with customer needs. However, the number of trips made by members is always higher than the casual riders across all months of the year.

<br/>

#### Visualizaton of average trip duration by user type on each day of the week

```{r}
all_trips_v2 %>%  
  group_by(usertype, day_of_week) %>% 
  summarise(average_trip_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = average_trip_duration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by user type Vs. Day of the week")
```

The average trip duration of a casual rider is more than twice that of a member. Note that this necessarily does not mean that casual riders travel farther distance. It is also interesting to note that weekends not only contribute to more number of trips but also longer trips on average when compared to weekdays.


<br/>

#### Visualizaton of average trip duration by customer type Vs. month
```{r}
all_trips_v2 %>%  
  group_by(usertype, month) %>% 
  summarise(average_trip_duration = mean(ride_length)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by user type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))
```

Average trip duration of member riders is anywhere between 8-15 minutes throughout the year. However, there seems to be a distinct pattern when it comes to casual riders, whose average trip duration swings wildly from as low as ~20 minutes to more than 30 minutes depending on time of the year. 

<br/>


#### Visualizaton of ride type Vs. number of trips by user type
```{r}
all_trips_v2 %>%
  group_by(ride_type, usertype) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= usertype))+
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
    labs(title ="Ride type Vs. Number of trips") +
    theme(axis.text.x = element_text(angle = 30))
```

Classic bikes and electric bikes are predominantly used by members. Docked bikes are almost never used compared to others and are only used by casual users. 

<br/>

### 6. Act

#### Important Findings

Members and casual riders differ in how long they use the bikes, how often they use the bikes, and on which days of the week does every group peak:

- Casual rides peak during weekends. There is a high probability they are tourists visiting and sightseeing the city, or that they are ordinary residents who are riding bike in their leisure time during the weekend. The longer average ride time for casual rider, also peaking at the weekend,  provides evidence for this point.

- Ride length for members are relatively shorter compared to casual riders. This could clarified as such, that most members use the bikes to commute on workdays. This clarification would also explain the short riding durations of members. They ride from point A to B, namely roughly always the same ride lengths and the same distance

- 59% of the riders are annual members, suggesting that the company have already achieved a certain level of loyalty among its bike users. This indicates a positive message, namely that the company is going to be able to convince many casual riders to convert to members, and to keep the new members satisfied. 

- Members prefer electric bikes and classic bikes the most while docked bikes are popular and only used among casual riders.

<br/>

### Recommendations

- As casual rider usage reach its highest point on the weekend, the marketing campaign can include weekend-only membership at a sensible price. This could attract casual riders to convert to members.

- The campaign could include ride-length-based tariff plan (maybe only on weekends): Bike more, pay less ! 
This provides more incentive for the member rides to cycle longer distances. 

### Additonal data that could expand scope of analysis

- Age and gender profile - Again, this data could be used to study the category of riders who can be targeted for attracting new members.

- Address/ neighborhood details of members to investigate if there are any location specific parameters that encourage membership.

- Pricing details for members and casual riders - Based on this data, we might be to optimize cost structure for casual riders or provide discounts without affecting the profit margin.

