#' Calculate SIR differential equations
#'
#' This function takes in 5 parameters and calculates the SIR differential equations based on the values provided.
#'
#' @param initial_susceptible the initial number of susceptible people within the closed population at time equal to 0.
#' @param initial_infected the initial number of infected people in the closed population at time equal to 0.
#' @param transmission_rate the transmission rate of the disease.
#' @param recovery_rate the recovery rate of the disease.
#' @param days the number of days the model calculates for.
#'
#' @return A data frame containing the simulated data for each day of the epidemic.
#'
#' @importFrom desolve "ode"
#' @export
#'
#' @examples
#' # write your examples here
#'




#SIR model function
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I

    return(list(c(dS, dI, dR)))
  })
}
#parameters
initial_state <- c(S = 0.99, I = 0.01, R = 0)
parameters <- c(beta = 0.3, gamma = 0.1)

times <- seq(0, 80, by = 1)

#SIR results
SIR_result <- function(initial_state, times, sir_model, parameters){
  output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
  return(as.data.frame(output))
}

#Example of function
SIR_result(initial_state, times, sir_model, parameters)

