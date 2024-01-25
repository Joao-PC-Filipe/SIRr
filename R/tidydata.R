
library(devtools)
library(usethis)
library(readr)
library(ggplot2)
library(dplyr)


WHO_COVID_19_global_data <- read_csv("DATASET/WHO-COVID-19-global-data.csv")
cov_data <- WHO_COVID_19_global_data

use_data(cov_data)


cov_data <- cov_data  %>% filter(Country == "Italy") %>% select(Date = Date_reported,New_daily_cases = New_cases,
                                                      Daily_deaths = New_deaths, total_deaths = Cumulative_deaths) %>%
                                                      mutate(day = 1:n())

early_data <- cov_data %>% filter(day<=200)

ggplot(early_data,aes(x = day, y = New_daily_cases)) + geom_line() +ylab("New Daily Cases")+
  ggtitle("Daily Cases in Italy (1st wave)") + scale_y_continuous(breaks =seq(0,7000,by=1000), limits =c(0,7000))

use_data(early_dat)
