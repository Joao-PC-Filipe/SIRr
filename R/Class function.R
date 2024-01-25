library(deSolve)
library(tidyverse)
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
SIR_result(initial_state, times, sir_model, parameters) {
  output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
  return(as.data.frame(output))
}

#' SIR Model Class
#'
#' This class represents the Susceptible-Infectious-Recovered (SIR) model.
#'
#' @importFrom methods setClass setMethod
#'
#' @name SIRModel
#' @slot initial_state A numeric vector representing the initial state of the system (S, I, R).
#' @slot parameters A numeric vector representing the parameters of the SIR model (beta, gamma).
#'
#' @exportClass SIRModel

#' Initialize SIR Model Object
#'
#' This method initializes an object of the SIRModel class with the provided initial state and parameters.
#'
#' @param initial_state A numeric vector representing the initial state of the system (S, I, R).
#' @param parameters A numeric vector representing the parameters of the SIR model (beta, gamma).
#'
#' @export

SIRModel <- function(S, I, R, beta, gamma) {
  structure(list(
    state = c(S = S, I = I, R = R),
    parameters = c(beta = beta, gamma = gamma)
  ), class = "SIRModel")
}

simulate <- function(object, times) {
  if(class(object) != "SIRModel") {
    stop("Object must be of class SIRModel")
  }


}
