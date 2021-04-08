# setup
rm(list = ls())

# libraries
library(tidyverse)
library(here)
library(lubridate)

# load data
data <- read_csv(here("Projects/deploys/deploys.csv"), col_names = c("date"))

# plot data (182 records)
daily <- data %>%
  group_by(date) %>%
  tally() 

# adding missing day and weekdays (421 records)
# reasonable to replace NA with zeros in this case
all <- daily %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>%
  mutate(
    weekday = as.factor(weekdays(date)),
    weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    number_of_deploys = n
    ) %>%
  replace_na(list(number_of_deploys = 0))

# plotting number of deployments per day
all %>%
  ggplot(aes(date, number_of_deploys)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Deployments by Flux per day for Tracker\n (February 12, 2020 to April 7, 2021)",
    x = "Date", 
    y = "Number of deployments"
  ) + 
  theme(
    plot.title = element_text(size=18, face="bold", margin=margin(20,0,20,0), hjust = 0.5, vjust = 0),
    axis.title.x = element_text(size=16, margin=margin(10,0,10,0)),
    axis.title.y = element_text(size=16, margin=margin(0,10,0,10)),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# plotting number of deployments per weekday
all %>%
  group_by(weekday) %>%
  summarize(
    deploys = sum(number_of_deploys)
    ) %>%
  ggplot(aes(deploys, reorder(weekday, desc(weekday)))) +
  geom_col() +
  theme_light() +
  labs(
    title = "Deployments by Flux per weekday for Tracker\n (February 12, 2020 to April 7, 2021)",
    x = "Number of deployments",
    y = "Weekday"
  ) + 
  theme(
    plot.title = element_text(size=18, face="bold", margin=margin(20,0,20,0), hjust = 0.5, vjust = 0),
    axis.title.x = element_text(size=16, margin=margin(10,0,10,0)),
    axis.title.y = element_text(size=16, margin=margin(0,10,0,10)),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  geom_text(aes(x=deploys, y=weekday, label=deploys, fill=NULL), nudge_x = 2)

# descriptive statistics
all %>%
  filter(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  summary()