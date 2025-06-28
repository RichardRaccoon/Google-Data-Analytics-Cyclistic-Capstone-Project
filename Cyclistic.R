
## install files for data cleaning

install.packages("dplyr",repos = "https://cran.r-project.org")
install.packages("skimr",repos = "https://cran.r-project.org")
install.packages("janitor",repos = "https://cran.r-project.org")
install.packages("here",repos = "https://cran.r-project.org")
install.packages("lubridate",repos = "https://cran.r-project.org")
install.packages("ggplot2",repos = "https://cran.r-project.org")
install.packages("knitr",repos = "https://cran.r-project.org")
library(dplyr)
library(skimr)
library(janitor)
library(here)
library(lubridate)
library(ggplot2)
library(knitr)
## import 12 months of data (4 quarters)

Q1 <- read.csv("~/Desktop/Cyclistic Files/Divvy_Trips_2020_Q1.csv")
Q2 <- read.csv("~/Desktop/Cyclistic Files/Divvy_Trips_2019_Q2.csv")
Q3 <- read.csv("~/Desktop/Cyclistic Files/Divvy_Trips_2019_Q3.csv")
Q4 <- read.csv("~/Desktop/Cyclistic Files/Divvy_Trips_2019_Q4.csv")

## Rename and standardise column names

Q1 <- rename(Q1,
        trip_id=ride_id,
        start_time_2 = started_at,
        end_time_2 = ended_at,
        usertype = member_casual
        )
Q4 <- rename(Q4,
        start_station_id = from_station_id,
        start_station_name = from_station_name,
        end_station_id  = to_station_id,
        end_station_name = to_station_name,
        end_time_2 = end_time,
        start_time_2 = start_time
        )
Q3 <- rename(Q3,
        start_station_id = from_station_id,
        start_station_name = from_station_name,
        end_station_id  = to_station_id,
        end_station_name = to_station_name,
        end_time_2 = end_time,
        start_time_2 = start_time
        )
Q2 <- rename(Q2,
        trip_id = X01...Rental.Details.Rental.ID,
        start_time_2 = X01...Rental.Details.Local.Start.Time,
        end_time_2 = X01...Rental.Details.Local.End.Time,
        bike_id = X01...Rental.Details.Bike.ID,
        trip_duration = X01...Rental.Details.Duration.In.Seconds.Uncapped,
        start_station_id = X03...Rental.Start.Station.ID,
        start_station_name = X03...Rental.Start.Station.Name,
        end_station_name = X02...Rental.End.Station.ID,
        end_station_id = X02...Rental.End.Station.Name,
        Member.Birth_Year = X05...Member.Details.Member.Birthday.Year,
        usertype = User.Type
        )

## Converted date from chr to ddtm

Q1$start_time <- as_datetime(Q1$start_time_2)
Q1$end_time <- as_datetime(Q1$end_time_2)
Q2$start_time <- as_datetime(Q2$start_time_2)
Q2$end_time <- as_datetime(Q2$end_time_2)
Q3$start_time <- as_datetime(Q3$start_time_2)
Q3$end_time <- as_datetime(Q3$end_time_2)
Q4$start_time <- as_datetime(Q4$start_time_2)
Q4$end_time <- as_datetime(Q4$end_time_2)

## Calculating trip duration and adding new columns for duration, as well as, day, month and season of travel


Q1v2 <- Q1%>%
  mutate(
    trip_duration = end_time - start_time,
    hours = hour(start_time),
    weekday = weekdays(start_time),
    month = month(start_time),
    quarter = quarters(start_time),
    usertype = ifelse(as.character(usertype) == "member", "Subscriber", as.character(usertype)),
    usertype = ifelse(as.character(usertype) == "casual", "Casual", as.character(usertype))
  )
Q2v2 <- Q2%>%
  mutate(
    trip_duration = end_time - start_time,
    hours = hour(start_time),
    weekday = weekdays(start_time),
    month = month(start_time),
    quarter = quarters(start_time),
    usertype = ifelse(as.character(usertype) == "Customer", "Casual", as.character(usertype))
  )
Q3v2 <- Q3%>%
  mutate(
    trip_duration = end_time - start_time,
    hours = hour(start_time),
    weekday = weekdays(start_time),
    month = month(start_time),
    quarter = quarters(start_time),
    usertype = ifelse(as.character(usertype) == "Customer", "Casual", as.character(usertype))
  )
Q4v2 <- Q4%>%
  mutate(
    trip_duration = end_time - start_time,
    hours = hour(start_time),
    weekday = weekdays(start_time),
    month = month(start_time),
    quarter = quarters(start_time),
    usertype = ifelse(as.character(usertype) == "Customer", "Casual", as.character(usertype))
  )

## Delete uncommon columns

Q1v2 <- Q1v2 [-c(2,3,4,9,10,11,12)]
Q2v2 <- Q2v2 [-c(2,3,4,11,12)]
Q3v2 <- Q3v2 [-c(2,3,4,5,11,12)]
Q4v2 <- Q4v2 [-c(2,3,4,5,11,12)]

## Reordering columns

Q1v2 <- Q1v2 %>% relocate(start_station_id, .before=start_station_name)
Q1v2 <- Q1v2 %>% relocate(end_station_id, .before=end_station_name)
Q2v2 <- Q2v2 %>% relocate(trip_duration, .after=end_time)

## Bind datasets together

all_qtrs <- rbind(Q1v2,Q2v2,Q3v2,Q4v2)


## checking mean for missing values (missing end_station_id) take out

mean(all_qtrs$trip_duration)
mean(all_qtrs$start_station_id)
mean(all_qtrs$end_station_id)

## selecting rows with N/A, "1" searches by row, "2" by column

rows_with_na <- all_qtrs[apply(
  all_qtrs,
  1,
  function(x) any(is.na(x))
  ),]

## delete row 414427 containing n/a

all_qtrs <- na.omit(all_qtrs)

## Removed rows with trip duration, equal to zero, negative,over 24 hours and those journeys which begin and end at the same station

all_qtrs <- all_qtrs[!(all_qtrs$trip_duration == 0),] 
all_qtrs <- all_qtrs[!(all_qtrs$trip_duration < 0),]
all_qtrs <- all_qtrs[!(all_qtrs$trip_duration > 86400),]
all_qtrs <- all_qtrs[!(all_qtrs$start_station_id == all_qtrs$end_station_id),]


## Calculating some simple stats for subscribers and casual users combined. 
## On the basis of findings I removed outliers (see above). 
##Firstly those trips over 24 hours as day pass runs out and those trips which began and ended at same station (all under 32 seconds)

all_qtrs %>%
  summarise(
    mean = mean(all_qtrs$trip_duration, na.rm = TRUE),
    max = max(all_qtrs$trip_duration, na.rm = TRUE),
    min = min(all_qtrs$trip_duration, na.rm = TRUE),
    sd = sd(all_qtrs$trip_duration,na.rm = TRUE)
  )
  
## mean, min and max trip duration by both subscriber and casual member

all_qtrs %>%
  group_by(usertype) %>%
  summarise(mean = mean(trip_duration),
            max = max(trip_duration),
            min = min(trip_duration),
            sd= sd(trip_duration)
  )

## average trip duration by weekday and usertype (similar pattern across all days)

all_qtrs %>%
  group_by(weekday, usertype) %>%
  summarise(
    Average_Trip = mean(trip_duration),
    Shortest_trip = min(trip_duration),
    Longest_trip = max(trip_duration),
    Standard_deviation=sd(trip_duration)
)

## trips by week (mode)

all_qtrs %>% 
  count(weekday)


## Figure 1: average weekly trip length (trip length longer for casual users so possibly offer bundles of single ride tickets at a discount)

trip_times <- all_qtrs %>%
  select(usertype, weekday, trip_duration) %>%
  group_by(usertype, weekday) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 
  trip_times$weekday <- factor(trip_times$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(
  data = trip_times, 
  aes(
    x = weekday, 
    y= average_trip_length,
    fill = usertype,
  )) +
  labs(
    title = "Trip lengths",
    subtitle = "Figure 1: Average trip lengths of subscribers and casual users",
    caption = "Data Source: Motivate International Inc."
  )+
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle =0))+
  ylab("Average Trip Length (secs)")+
  xlab("")+
  labs(fill="Customer Type")
  ggsave("~/Desktop/Cyclistic Files/figure_1.jpg")
  

## figure 2: chart showing total trips by weekday and usertype. Recommendation: offer saturday and sunday membership for casual users

weekly_travel <- all_qtrs %>%
  select(usertype, weekday, trip_duration) %>%
  group_by(usertype,weekday) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 
weekly_travel$weekday <- factor(weekly_travel$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(
  data = weekly_travel, 
  aes(
    x = weekday, 
    y= total_trips,
    fill = usertype,
  )) +
  labs(
    title = "Number of Trips Each Day",
    subtitle = "Figure 2: Comparison of trip numbers by weekday and user",
    caption = "Data Source: Motivate International Inc."
  )+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle =360))+
  ylab("Trip Numbers")+
  xlab("")+
  labs(fill="Customer Type")+
  scale_y_continuous(labels = scales::comma)
  ggsave("~/Desktop/Cyclistic Files/figure_2.jpg")

## figure 3: chart showing total trips by month and usertype.Recommendation: Offer casual riders a seasonal pass 6mths May to Sept or 4 mnths June to August

monthly_travel <- all_qtrs %>%
  select(usertype, month, trip_duration) %>%
  group_by(usertype, month) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 

ggplot(
  data = monthly_travel, 
  aes(
    x = month, 
    y= total_trips,
    fill = usertype,
  ))+
  labs(
    title = "Numbers of Trips Each Month",
    subtitle = "Figure 3: Comparison of trip numbers by month and user",
    caption = "Data Source: Motivate International Inc."
  ) +
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle =45))+
  ylab("Trip Numbers")+
  xlab("")+
  labs(fill="Customer Type")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name)
  ggsave("~/Desktop/Cyclistic Files/figure_3.jpg")

## figure 4: chart showing total trips by weekday, month and usertype. Recommendation: offer saturday and sunday membership for casual users

weekly_travel <- all_qtrs %>%
  mutate (monthname = month.name[month])%>%
  select(usertype, weekday, month,monthname, trip_duration) %>%
  group_by(usertype, weekday, month,monthname) %>%
  summarise (
    average_trip_length = mean(trip_duration), 
    total_trips = n()) 
  weekly_travel$weekday <- factor(weekly_travel$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  weekly_travel$monthname <- factor(weekly_travel$monthname, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
  
ggplot(
  data = weekly_travel, 
  aes(
    x = weekday, 
    y= total_trips,
    fill = usertype,
    )) +
    labs(
      title = "Trip Numbers Each Month and Day",
      subtitle = "Figure 4: Comparison of trip numbers by month, weekday and user",
      caption = "Data source: Motivate International Inc.")+
    geom_col(position = "dodge")+
    theme(axis.text.x = element_text(angle =90))+
    theme(axis.text.x = element_text(vjust = 0.25))+
    ylab("Trip Numbers")+
    xlab("")+
    labs(fill="Customer Type")+
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~monthname)
  ggsave("~/Desktop/Cyclistic Files/figure_4.jpg")

## Figure 5 This works - 24 hour pattern

daily_travel <- all_qtrs %>%
  select(usertype, hours, trip_duration) %>%
  group_by(usertype, hours) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 

ggplot(
  data = daily_travel, 
  aes(
    x = hours, 
    y= total_trips,
    fill = usertype,
  )) +
  labs(
    title = "Number of Hourly Trips",
    subtitle = "Figure 5: Chart shows 24 hour pattern by usertype",
    caption = "Data Source: Motivate International Inc."
  )+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle =0))+
  ylab("Trip Numbers")+
  xlab("Hour of Day")+
  labs(fill="Customer Type")+
  scale_y_continuous(labels = scales::comma)
ggsave("~/Desktop/Cyclistic Files/figure_5.jpg")

## Figure 6: Faceted, it's clear that user pattern is similar on saturdays and sundays for casual users and subscribers

daily_travel <- all_qtrs %>%
  select(usertype, hours,weekday, trip_duration) %>%
  group_by(usertype, hours, weekday) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 
daily_travel$weekday <- factor(daily_travel$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(
  data = daily_travel, 
  aes(
    x = hours, 
    y= total_trips,
    fill = usertype,
  )) +
  labs(
    title = "Number of Hourly Trips During a Week",
    subtitle = "Figure 6: Chart shows 24 hour pattern by usertype over the course of a week",
    caption = "Data source: Motivate International Inc."
  )+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle =0))+
  ylab("Trip Numbers")+
  xlab("Hour of Day")+
  labs(fill="Customer Type")+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~weekday)
  ggsave("~/Desktop/Cyclistic Files/figure_6.jpg")

##Figure 7: WiP split between day passes and single trip casual users

pass_type <- all_qtrs%>%
  filter(usertype == "Casual")%>%
  select(usertype,trip_duration,trip_id)%>%
  mutate(trip_duration_seconds = (as.numeric(trip_duration/60)))%>%
  mutate(trip_duration_grouped = cut(trip_duration_seconds, c(0,30,60,90,120,150,180,Inf)))%>%
  group_by(trip_duration_grouped)

pass_type_2 <- pass_type%>%
  select(trip_duration_grouped,usertype)%>%
  group_by(trip_duration_grouped)%>%
  summarise (count=n())

ggplot(
  data = pass_type_2, 
  aes(
    x = trip_duration_grouped, 
    y= count,
    fill = "red",
  )) +
  labs(
    title = "Trip Durations by Casual Users",
    subtitle = "Figure 7: shows most casual users take trips less than 90 minutes",
    caption = "Data Source: Motivate International Inc."
  )+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 360))+
  ylab("Trip Numbers")+
  xlab("Trip Duration (hours)")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-24"))
  
legend.position = "none"
ggsave("~/Desktop/Cyclistic Files/figure_7.jpg")

## Figure 8: Identify most used starting stations (with slice and arrange to select top 15 stations)> RL - calculate average??? Shows that Casual users mainly use two stations 

top_stations <- all_qtrs %>%
  select(usertype, weekday, trip_duration, start_station_name) %>%
  group_by(start_station_name,usertype,weekday) %>%
  summarise(
    total_trips = n(),
  )
  top_stations %>%
    arrange(-total_trips) %>%
    ungroup(start_station_name,usertype) %>%
    slice_max(total_trips, n = 15) %>%
  group_by(start_station_name,usertype,weekday)

  ggplot(
    data = top_stations, 
    aes(
      x = start_station_name, 
      y= total_trips,
      fill = usertype,
    )) + 
    labs(
      title = "Frequency of station use at start of journeys",
      subtitle = "Figure 8: shows two stations are used frequently by casual users",
      caption = "Data Source: Motivate International Inc.")+
    geom_col(position = "dodge") +
    theme(axis.text.x = element_blank())+
    ylab("Total Trips")+
    xlab("Start Station")+
    labs(fill="Customer Type")
  ggsave("~/Desktop/Cyclistic Files/figure_8.jpg")
  
## Figure 9: Identify most popular starting stations. Recommendation: Focus advertising and focus on customer engagement at these stations

top_18_stations <- all_qtrs %>%
  select(usertype, weekday, start_station_name) %>%
  group_by(start_station_name,usertype) %>%
  summarise(
    total_trips = n(),
  )
top_18_stations %>%
  arrange(-total_trips)
xx <- top_18_stations[!(top_18_stations$total_trips < 20000),] 

ggplot(
  data = xx, 
  aes(
    x = start_station_name, 
    y= total_trips,
    fill = usertype,
  )) + 
  labs(
    title = ("Most Popular Origin Stations"),
    subtitle = ("Figure 9: Chart identifies stations where journeys begin"),
    caption = ("Data Source: Motivate International Inc.")
  )+
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(hjust = 1))+
  theme(axis.text.x = element_text(vjust = 0.5))+
    xlab("Start Station")+
    ylab("Total Trips")+
    labs(fill="Customer Type")
  ggsave("~/Desktop/Cyclistic Files/figure_9.jpg")

## Figure 10: Identify most popular ending stations.

end_stations <- all_qtrs %>%
  select(usertype, weekday, trip_duration, end_station_name) %>%
  group_by(end_station_name,usertype) %>%
  summarise(
    total_trips = n(),
  )
end_stations %>%
  arrange(-total_trips)
yy <- end_stations[!(end_stations$total_trips < 15000),] 

ggplot(
  data = yy, 
  aes(
    x = end_station_name, 
    y= total_trips,
    fill = usertype,
  )) +  
  labs(
    title = ("Most Popular Destinations"),
    subtitle = ("Figure 10: Chart identifies stations where most journeys end"),
    caption = ("Data Source: Motivate International Inc.")
  )+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(hjust = 1))+
  theme(axis.text.x = element_text(vjust = 0.5))+
  xlab("Start Station")+
  ylab("Total Trips")+
  labs(fill= "Customer Type")
  ggsave("~/Desktop/Cyclistic Files/figure_10.jpg")

##Figure 11: Identify most popular starting stations. Recommendation: Focus advertising and focus on customer engagement at these stations

top_stations <- all_qtrs %>%
  select(usertype, weekday, trip_duration, start_station_name) %>%
  group_by(start_station_name,usertype,weekday) %>%
  summarise(
    total_trips = n(),
  )
top_stations %>%
  arrange(-total_trips)
xx <- top_stations[!(top_stations$total_trips < 4000),] 

ggplot(
  data = xx, 
  aes(
    x = start_station_name, 
    y= total_trips,
    fill = usertype,
  )) + 
  labs(
    title = ("Most Popular Origin Stations by Day"),
    subtitle = ("Figure 11: Chart shows stations where most journeys begin by day of week"),
    caption = ("Data Source: Motivate International Inc.")
  )+
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(hjust = 1))+
  theme(axis.text.x = element_text(vjust = 0.5))+
  xlab("Start Station")+
  ylab("Total Trips")+
  labs(fill="Customer Type")+
  facet_wrap(~weekday)
  ggsave("~/Desktop/Cyclistic Files/figure_11.jpg")

## average trip duration by start station

all_qtrs %>%
    group_by(start_station_name,usertype) %>%
    summarise(mean_trip_duration = mean(trip_duration))%>%
    arrange(-mean_trip_duration)

## average trip duration by end station (mmm...?)

all_qtrs %>%
  group_by(end_station_name,usertype) %>%
  summarise(mean_trip_duration = mean(trip_duration))%>%
  arrange(-mean_trip_duration)

 
## ........min trip duration by subscriber / member (use)

all_qtrs %>%
  slice_min(trip_duration, n = 100, with_ties = TRUE)

## ........max trip duration by subscriber / member (use)

all_qtrs %>%
  slice_max(trip_duration, n = 100, with_ties = TRUE)

## filter for journeys of 0 length and negative 

bike0 <- bike_rides %>%
  filter(ride_length == 0)

bike1 <- bike_rides %>%
  filter(ride_length < 0)

## count trips by weekday and usertype (shown in a chart later)

all_qtrs %>% 
  count(weekday,usertype)

## count most frequently used start station

all_qtrs %>% 
  count(start_station_name, usertype)%>%
  arrange(-n)

## count most frequently used end station

all_qtrs %>% 
  count(end_station_name, usertype)%>%
  arrange(-n)


## not needed: chart showing total trips by quarter and usertype.Recommendation: same as above

quarterly_travel <- all_qtrs %>%
  select(usertype, quarter, trip_duration) %>%
  group_by(usertype, quarter) %>%
  summarise(average_trip_length = mean(trip_duration), 
            total_trips = n()) 

ggplot(
  data = quarterly_travel, 
  aes(
    x = quarter, 
    y= total_trips,
    fill = usertype,
  )) +
  labs(
    title = "Total Numbers of Quarterly Trips 2019-20",
    subtitle = "Chart shows pattern of quarterly use by subscribers and casual users of cyclistic bikes",
    caption = "Richard Laffar, September '24"
  ) +
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle =360))+
  ylab("Trip Numbers")+
  xlab("Quarter")+
  labs(fill="Customer Type")

## Not neededfy top 10 stations (with slice and arrange to select top 15 stations)> RL - calculate average??? Shows that Casual users mainly use two stations 

top_15_stations <- all_qtrs %>%
  select(usertype, weekday, trip_duration, start_station_name) %>%
  group_by(start_station_name,usertype,weekday) %>%
  summarise(
    total_trips = n(),
  )
top_15_stations %>%
  arrange(-total_trips) %>%
  ungroup(start_station_name,usertype) %>%
  slice_max(total_trips, n = 15) %>%
  group_by(start_station_name,usertype,weekday)

ggplot(
  data = top_15_stations, 
  aes(
    x = start_station_name, 
    y= total_trips,
    fill = usertype,
  )) + 
  labs(
    title = "Most used stations at start of journeys 2019-20",
    subtitle = "Trip frequency by start station",
    caption = "Richard Laffar, September '24"
  )+
  geom_col(position = "dodge") +
  theme(axis.text.x = element_blank())+
  ylab("Total Trips")+
  xlab("Start Station")+
  labs(fill="Customer Type")+
  facet_wrap(~weekday)

##Figure 7a: WiP split between day passes and single trip casual users

AQ2<- all_qtrs%>%
  filter(usertype == "Casual")%>%
  select(usertype,trip_duration,trip_id)%>%
  mutate(trip_duration_seconds = (as.numeric(trip_duration/60)))%>%
  mutate(trip_duration_grouped = cut(trip_duration_seconds, c(0,30,60,90,120,150,180,Inf)))%>%
  group_by(trip_duration_grouped)

AQ3 <- AQ2%>%
  select(trip_duration_grouped,usertype)%>%
  group_by(trip_duration_grouped)%>%
  summarise (count=n())

ggplot(data = AQ3, aes(x="", y=count, fill=trip_duration_grouped ))+
  geom_bar(stat = "identity")+
  coord_polar("y")+
  theme_void()+
  labs(
    title = "Trip Durations by Casual Users",
    subtitle = "Figure 7: shows most casual users take trips less than 90 minutes",
    caption = "Data Source: Motivate International Inc.",
    fill = "Trip Duration (mins)"
  )+
  theme(plot.caption = element_text(hjust = 1.5))+
  scale_fill_discrete(labels = c("<30","30-59","60-89","90-119","120-149","150-179","180+"))+
  ggsave(filename = "~/Desktop/Cyclistic Files/figure_7a.jpg",width = 5, height = 5)
