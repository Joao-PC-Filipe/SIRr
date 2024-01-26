#' SIRResult Class Definition
#' @title Defines a class to represent the results of the SIR model simulation.
#' @description Represents the results of the SIR model simulation.
#' @export SIRResult
#' @author "Joao Filipe" "Siun Mulcahy" "Ellen Creed"
#' @importFrom methods "setMethod" "setGeneric" "setClass"
#' @importFrom deSolve "ode"
#' @importFrom devtools "check"
#' @keywords "class" "SIRResult"
#' @seealso \code{\link{initialize.SIRResult}}, \code{\link{generate.SIRResult}}

#' Constructor method for the SIRResult class
#' @title Initializes an object of class SIRResult.
#' @description Initializes an object of class SIRResult.
#' @param .Object An object of class SIRResult.
#' @param initial_state Numeric vector representing the initial state of the system.
#' @param times Numeric vector representing the time points for simulation.
#' @param sir_model A function defining the SIR model dynamics.
#' @param parameters Numeric vector representing the parameters of the SIR model.
#' @return An object of class SIRResult.
#' @keywords methods initialize SIRResult
#' @export
#' @author "Joao Filipe" "Siun Mulcahy" "Ellen Creed"

#' Generate SIR results
#' @title Generates SIR model simulation results.
#' @description Generates SIR model simulation results based on the specified parameters.
#' @param object An object of class SIRResult.
#' @return An updated object of class SIRResult with the simulation results.
#' @keywords methods generate SIRResult
#' @export
#' @author "Joao Filipe" "Siun Mulcahy" "Ellen Creed"
#' @examples
#' # Define initial state and parameters
#' initial_state <- c(S = 0.99, I = 0.01, R = 0)
#' parameters <- c(beta = 0.3, gamma = 0.1)
#'
#' # Define time points
#' times <- seq(0, 80, by = 1)
#'
#' # Create an object of class SIRResult
#' sir_result_object <- new("SIRResult",
#'                          initial_state = initial_state,
#'                          times = times,
#'                          sir_model = function(time, state, parameters) {
#'                            with(as.list(c(state, parameters)), {
#'                              dS <- -beta * S * I
#'                              dI <- beta * S * I - gamma * I
#'                              dR <- gamma * I
#'                              return(list(c(dS, dI, dR)))
#'                            })
#'                          },
#'                          parameters = parameters)
#'
#' # Generate the SIR results
#' sir_result_object <- generate(sir_result_object)
#'
#' # View the output
#' print(sir_result_object@output)


library(methods)
library(deSolve)
library(devtools)

initial_state <- c(S = 0.99, I = 0.01, R = 0)
parameters <- c(beta = 0.3, gamma = 0.1)
times <- seq(0, 80, by = 1)


#Define the SIRResult class
setClass("SIRResult",
         representation(
           initial_state = "numeric",
           times = "numeric",
           sir_model = "function",
           parameters = "numeric",
           output = "data.frame"
         ))

#Define a constructor method for the SIRResult class
setMethod("initialize",
          signature("SIRResult"),
          function(.Object, initial_state, times, sir_model, parameters) {
            .Object@initial_state <- initial_state
            .Object@times <- times
            .Object@sir_model <- sir_model
            .Object@parameters <- parameters
            return(.Object)
          })

setGeneric("generate", function(object) standardGeneric("generate"))


#Define a method to generate the SIR results
setMethod("generate",
          signature = "SIRResult",
          function(object) {
            output <- ode(y = object@initial_state, times = object@times, func = object@sir_model, parms = object@parameters)
            object@output <- as.data.frame(output)
            return(object)
          })


sir_result_object <- new("SIRResult",
                         initial_state = initial_state,
                         times = times,
                         sir_model = function(time, state, parameters) {
                           with(as.list(c(state, parameters)), {
                             dS <- -beta * S * I
                             dI <- beta * S * I - gamma * I
                             dR <- gamma * I
                             return(list(c(dS, dI, dR)))
                           })
                         },
                         parameters = parameters)

sir_result_object <- generate(sir_result_object)

print(sir_result_object@output)

devtools::check()
