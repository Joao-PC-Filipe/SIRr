

# 1. Calculating the cumulative infected individuals
# Each row contains total number of infected individuals up to that date
early_data$Cumulative_cases <- cumsum(early_data$New_daily_cases)

#2. Average infection period for covid
infection_duration <- 14

#3. Calculating cumulative cases and deaths
early_data$Cumulative_cases <- cumsum(early_data$New_daily_cases)
early_data$Cumulative_deaths <- cumsum(early_data$Daily_deaths)

#4. Estimating recovered individuals
early_data$Estimated_recovered <- lag(early_data$Cumulative_cases, infection_duration, default = 0) - early_data$Cumulative_deaths


#5. Total population
total_population <- 59440000

#6. Initial number of infected
initial_infected <- early_data$New_daily_cases[which(early_data$New_daily_cases > 0)[1]]

#7. Initial recovered
initial_recovered <- 0

# Estimating initial susceptible population
initial_susceptible <- total_population - initial_infected - initial_recovered

##################
# Convert Cumulative Cases and Deaths to Proportions
early_data$Proportion_Cumulative_cases <- cumsum(early_data$New_daily_cases) / total_population
early_data$Proportion_Cumulative_deaths <- cumsum(early_data$Daily_deaths) / total_population

# Estimating Proportion of Recovered Individuals
early_data$Proportion_Estimated_recovered <- lag(early_data$Proportion_Cumulative_cases, infection_duration, default = 0) - early_data$Proportion_Cumulative_deaths

# Calculating the Proportion of Currently Infected
early_data$Proportion_Infected <- early_data$New_daily_cases / total_population

# Calculating the Proportion of Susceptible Individuals
early_data$Proportion_Susceptible <- 1 - early_data$Proportion_Cumulative_cases - early_data$Proportion_Estimated_recovered

# Creating the S, I, R columns
early_data$S <- early_data$Proportion_Susceptible
early_data$I <- early_data$Proportion_Infected
early_data$R <- early_data$Proportion_Estimated_recovered

##############
#Testing model

#SIR model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I

    return(list(c(dS, dI, dR)))
  })
}

#SIR result
SIR_result <- function(initial_state, times, sir_model, parameters){
  output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
  return(as.data.frame(output))
}

library(deSolve)

# Initial conditions from the first day of data
initial_state <- c(S = early_data$S[1], I = early_data$I[1], R = early_data$R[1])

# 200 days of data ############ NO LONGER 200 DAYS OF DATA ############
########## It's now 150 days after starting on day 51 due to observations with zeros
times <- seq(0, 200, by = 1) #that's 201 numbers not 200

# Estmated parameters
parameters <- c(beta = 0.3, gamma = 0.9)

# Running the SIR model
Model_sample <- SIR_result(initial_state = initial_state, times = times, sir_model= sir_model, parameters = parameters)

