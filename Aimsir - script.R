# AIMSIR
# 12/2/26
# DATA SOURCE: https://www.met.ie/ga/climate/available-data/historical-data
# DOWNLOAD DAILY DATA SERIES FROM WEATHER STATION OF CHOICE

# TO DO:
# - FIX DATE ISSUE? //
# - CONVERT KNOTS TO KM/H //
# - COMPARE YEARS
# - TURN INTO FUNCTIONS
# - TURN INTO SHINY APP

library(tidyverse)

# WHERE IS YOUR DATA?
setwd("C:/Users/Admin/OneDrive/Documents/R stuff/aimsir project")

# READ IN LEGEND
DunsanyLegend <- read.csv("dly1375.csv", nrows = 22)
# DATA ONLY
AllDunsany <- read.csv("dly1375.csv", skip = 23)
View(AllDunsany)

# CHANGE DATE FORMAT
AllDunsany$date <- as.Date(AllDunsany$date, format = "%d-%b-%Y")
AllDunsany <- AllDunsany %>%
  mutate(month = month(date), year = year(date), day = day(date))

# CONVERT KNOTS TO KM/H
AllDunsany <- AllDunsany %>%
  mutate(wdspkm = wdsp * 1.85200)

# ONE YEAR
D2025 <- AllDunsany %>%
  filter(year == 2025)

# BASIC DAILY RAIN YEAR
AllDunsany %>%
  filter(year == 2026, month == 1) %>%
  ggplot(aes(x = date, y = rain)) +
  geom_col() +
  ylim(0, 20)



############################################
# GET MONTH AND YEAR FOR GRAPHS
time.labeller <- function(m, y) {
  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")
  
  month_text <- if(length(m) == 1)
    month_names[m]
  else
    paste0(month_names[min(m)], "-", month_names[max(m)])
  
  year_text <- if(length(y) == 1)
    as.character(y)
  else
    paste0(min(y), "-", max(y))
  
  paste(month_text, year_text)
}



#### AVERAGE DAILY FOR JANUARY ACROSS ALL YEARS ####
# 4 VARIABLES AS BAR CHART
bar.by.month <- function(m = 1, y = 2006:2026) {
graph <- AllDunsany %>%
  filter(month %in% m, year %in% y) %>%
  group_by(year) %>%
  summarise(Rain = mean(rain, na.rm = TRUE),
            Sun = mean(glorad, na.rm = TRUE),
            Wind = mean(wdspkm, na.rm = TRUE),
            Temp = mean(maxtp, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Rain, Sun, Wind, Temp), 
               names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("Temp", "Rain", "Wind", "Sun"))) %>%
  ggplot(aes(x = year, y = value, fill = variable)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_y",
             labeller = as_labeller(c(
               Rain = "Rain (mm)",
               Sun = "Sun (J/cm²)",
               Wind = "Wind (km/h)",
               Temp = "Temp (°C)"))) +
  scale_fill_manual(values = c(Temp = "#F8766D",
                               Rain = "#00BFC4",
                               Wind = "#C77CFF",
                               Sun = "#CD9600")) +
  labs(x = "Year", title = paste(time.labeller(m, y), "Weather Averages, Dunsany")) +
  theme(legend.position = "none", axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
return(graph)
}
bar.by.month(m = 1, y = 2006:2026)



############################################
# ALL 4 LINES IN ONE
AllDunsany %>%
  filter(month == 1) %>%
  group_by(year) %>%
  summarise(Temp = mean(maxtp, na.rm = TRUE),
            Rain = mean(rain, na.rm = TRUE),
            Wind = mean(wdspkm, na.rm = TRUE),
            Sun = mean(glorad, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Rain, Wind, Temp, Sun), 
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(Temp = "#F8766D", Rain = "#00BFC4",
                                 Wind = "#C77CFF",Sun = "#CD9600"),
                      labels = c(Temp = "Temp (°C)", Rain = "Rain (mm)",
                                 Wind = "Wind (km/h)", Sun = "Sun (J/cm²)")) +
  labs(x = "Year", , title = "January Weather Averages") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))



############################################
# ALL 4 LINES WITH FACET WRAP
AllDunsany %>%
  filter(month == 1) %>%
  group_by(year) %>%
  summarise(Temp = mean(maxtp, na.rm = TRUE),
            Rain = mean(rain, na.rm = TRUE),
            Sun = mean(glorad, na.rm = TRUE),
            Wind = mean(wdspkm, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Rain, Sun, Wind, Temp), 
               names_to = "variable", values_to = "value") %>%
  mutate(panel = ifelse(variable == "Sun", "Sun", "Other")) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y", , strip.position = "left") +
  scale_colour_manual(values = c(Temp = "#F8766D", Rain = "#00BFC4",
                                 Wind = "#C77CFF", Sun = "#CD9600"),
                      labels = c(Temp = "Temp (°C)", Rain = "Rain (mm)",
                                 Sun = "Sun (J/cm²)", Wind = "Wind (km/h)")) +
  labs(x = "Year", title = "January Weather Averages") +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


