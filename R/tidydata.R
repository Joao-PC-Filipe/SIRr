
library(devtools)
library(usethis)
library(readr)
library(ggplot2)
library(dplyr)

WHO_COVID_19_global_data <- read_csv("DATASET/WHO-COVID-19-global-data.csv")

load_data <- function(DATA_csv = WHO_COVID_19_global_data){
cov_data <- WHO_COVID_19_global_data

if (!(file.exists('data/cov_data.rda'))) use_data(cov_data)

cov_data <- cov_data  %>% filter(Country == "Italy") %>% select(Date = Date_reported,New_daily_cases = New_cases,
                                                      Daily_deaths = New_deaths, total_deaths = Cumulative_deaths) %>%
                                                      mutate(day = 1:n())

early_data <- cov_data %>% filter(50 < day & day <=200)

early_data$Cumulative_cases <- cumsum(early_data$New_daily_cases)
early_data$Cumulative_deaths <- cumsum(early_data$Daily_deaths)

#2. Average infection period for covid
infection_duration <- 14

#4. Estimating recovered individuals
early_data$Estimated_recovered <- lag(early_data$Cumulative_cases, infection_duration, default = 0)

#5. Total population
total_population <- 59440000/10000

#6. Initial number of infected
initial_infected <- early_data$New_daily_cases[which(early_data$New_daily_cases > 0)[1]]

#7. Initial recovered
initial_recovered <- 0

# Estimating initial susceptible population
initial_susceptible <- total_population - initial_infected - initial_recovered

##################
# Convert Cumulative Cases and Deaths to Proportions
early_data$Proportion_Cumulative_cases <- early_data$Cumulative_cases / total_population
early_data$Proportion_Cumulative_deaths <- early_data$Cumulative_deaths / total_population

# Estimating Proportion of Recovered Individuals
early_data$Proportion_Estimated_recovered <- lag(early_data$Proportion_Cumulative_cases, infection_duration, default = 0)

# Calculating the Proportion of Currently Infected
#### This is the proportion of population infected daily not the full proportion
#####of infected population at a given time####
#early_data$Proportion_Infected <- early_data$New_daily_cases / total_population

early_data$Proportion_Infected <- (early_data$Cumulative_cases - early_data$Cumulative_deaths-
early_data$Estimated_recovered)/ total_population

# Calculating the Proportion of Susceptible Individuals
#### Estimated recovered is just the cumulative cases lagged by 14 days so by subtracting
#### twice we are double counting away from the susceptible population
early_data$Proportion_Susceptible <- 1 - early_data$Proportion_Cumulative_cases

# Creating the S, I, R columns
early_data$S <- early_data$Proportion_Susceptible
early_data$I <- early_data$Proportion_Infected
early_data$R <- early_data$Proportion_Estimated_recovered

ggplot(early_data,aes(x = day, y = New_daily_cases)) + geom_line() +ylab("New Daily Cases")+
  ggtitle("Daily Cases in Italy (1st wave)") + scale_y_continuous(breaks =seq(0,7000,by=1000), limits =c(0,7000))

if (!(file.exists('data/cov_data.rda'))) use_data(early_data)

}

load_data()

