#setwd("E:/Summer-2024/Independent Study/R programming")

library(tidyverse)
library(lubridate) # For working with dates


mesonet <- read.csv("OKCE.csv")
head(mesonet)
str(mesonet)
unique(mesonet$RAIN)


#missing value

# Replace -998.00 and -996.00 with NA
mesonet_NA <- mesonet %>%
  mutate(
    TAVG = ifelse(TAVG %in% c(-998.00, -996.00), NA, TAVG),
    HAVG = ifelse(HAVG %in% c(-998.00, -996.00), NA, HAVG),
    RAIN = ifelse(RAIN %in% c(-998.00, -996.00), NA, RAIN)
  )

# Remove rows with NA values in any of the specified columns
mesonet_clean <- mesonet_NA %>%
  filter(!is.na(TAVG) & !is.na(HAVG) & !is.na(RAIN))


#Date in one column
mesonet_Date <- mesonet_clean %>%
  mutate(Date = as.Date(paste(YEAR, MONTH, DAY, sep = "-")))

##barplot(mesonet_clean$RAIN)



# Create a data frame with file Date & organize df
mesonet_FData <- data.frame(mesonet_Date) %>%                     
                        select(-YEAR,-MONTH,-DAY) %>%
                        select(Date,STID,everything())



# Define a function to determine the season based on the month
get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}


# Add a season column to the data frame

# Use mutate() to add a season column to the data frame

mesonet_season <- mesonet_FData %>%
  mutate(Season = sapply(Date, get_season))

################### Extract the year from the Date column

mesonet_season <- mesonet_season %>%
  mutate(Year = year(Date))

## EDA

hist(mesonet_season$TAVG)
boxplot(mesonet_season$TAVG)



#######################  Plot TAVG according to year and particular season

ggplot(mesonet_season, aes(x = Year, y = TAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Avg Temperature (°F) in OKCE",
       x = "Year",
       y = "TAVG (°F)") +
  theme_minimal() +
  scale_color_manual(values = c( "Summer" = "red"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals



######## Plot TAVG according to year and four seasons

ggplot(mesonet_season, aes(x = Year, y = TAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Avg Temperature (°F) in OKCE",
       x = "Year",
       y = "TAVG  (°F)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals



############ Seasonal avg
######## Use mutate() to add a season column to the data frame

mesonet_season <- mesonet_FData %>%
  mutate(Season = sapply(Date, get_season)) %>%
  mutate(Year = year(Date))

seasonal_avg <- mesonet_season %>%
  group_by(Year, Season) %>%
  summarize(TAVG = mean(TAVG, na.rm = TRUE))

############ Seasonal avg visualization

ggplot(seasonal_avg, aes(x = Year, y = TAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = Season)) +
  labs(title = "Seasonal Average Temperature  (°F) in OKCE",
       x = "Year",
       y = "TAVG (°F)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals




###################   HAVG   ##################################

################### HAVG: Extract the year from the Date column


## EDA

hist(mesonet_season$HAVG)
boxplot(mesonet_season$HAVG)

#######################  Plot HAVG according to year and particular season

ggplot(mesonet_season, aes(x = Year, y = HAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Average Humidity (%) in OKCE",
       x = "Year",
       y = "HAVG (%)") +
  theme_minimal() +
  scale_color_manual(values = c( "Summer" = "red"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals



######## Plot HAVG according to year and four seasons

ggplot(mesonet_season, aes(x = Year, y = HAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Average Humidity (%) for Different Seasons in OKCE",
       x = "Year",
       y = "HAVG (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals


############ Seasonal avg
######## Use mutate() to add a season column to the data frame

mesonet_season <- mesonet_FData %>%
  mutate(Season = sapply(Date, get_season)) %>%
  mutate(Year = year(Date))

seasonal_avg <- mesonet_season %>%
  group_by(Year, Season) %>%
  summarize(HAVG = mean(HAVG, na.rm = TRUE))


############ Seasonal avg visualization

ggplot(seasonal_avg, aes(x = Year, y = HAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = Season)) +
  labs(title = "Seasonal Average Humidity (%) in OKCE",
       x = "Year",
       y = "HAVG (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals








###################   RAIN   ##################################

################### RAIN: Extract the year from the Date column


## EDA

hist(mesonet_season$RAIN)
boxplot(mesonet_season$RAIN)

#######################  Plot RAIN according to year and particular season

ggplot(mesonet_season, aes(x = Year, y = RAIN, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Rain (inches) in OKCE",
       x = "Year",
       y = "Rain (inches)") +
  theme_minimal() +
  scale_color_manual(values = c( "Summer" = "red"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals



######## Plot RAIN according to year and four seasons

ggplot(mesonet_season, aes(x = Year, y = RAIN, color = Season)) +
  geom_point() +
  geom_line(aes(group = interaction(Year, Season))) +
  labs(title = "Rain (inches) for Different Seasons in OKCE",
       x = "Year",
       y = "RAIN (inches)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals


############ Seasonal avg
######## Use mutate() to add a season column to the data frame

mesonet_season <- mesonet_FData %>%
  mutate(Season = sapply(Date, get_season)) %>%
  mutate(Year = year(Date))

seasonal_avg <- mesonet_season %>%
  group_by(Year, Season) %>%
  summarize(RAIN = mean(RAIN, na.rm = TRUE))


############ Seasonal avg visualization

ggplot(seasonal_avg, aes(x = Year, y = RAIN, color = Season)) +
  geom_point() +
  geom_line(aes(group = Season)) +
  labs(title = "Seasonal Average Rain (inches) in OKCE",
       x = "Year",
       y = "RAIN (inches)") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange"))+
  scale_x_continuous(breaks = seq(min(mesonet_season$Year), max(mesonet_season$Year), by = 2))  # Setting 2-year intervals







##################################################### rough part for test


mesonet_season <- mesonet_season %>%
  mutate(Year = year(Date))

# Calculate the average temperature for each year and season
seasonal_avg <- mesonet_season %>%
  group_by(Year, Season) %>%
  summarize(TAVG = mean(TAVG, na.rm = TRUE))

# Plot the seasonal average temperature over the years with 2-year intervals
ggplot(seasonal_avg, aes(x = Year, y = TAVG, color = Season)) +
  geom_point() +
  geom_line(aes(group = Season)) +
  #geom_smooth(method = "lm", se = FALSE) +  # Adding a trend line
  labs(title = "Seasonal Average Temperature in OKCE",
       x = "Year",
       y = "TAVG") +
  theme_minimal() +
  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange")) +
  scale_x_continuous(breaks = seq(min(seasonal_avg$Year), max(seasonal_avg$Year), by = 2))  # Setting 2-year intervals






