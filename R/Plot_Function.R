#' Plotting function for SIR model simulation results
#'
#' This function creates a line plot to visualize the simulation results of the SIR (Susceptible-Infectious-Recovered) model.
#'
#' @param output_df A data frame containing the simulation results with columns 'time', 'S', 'I', 'R'.
#' @param title A character string specifying the title of the plot. Default is "SIR Model Simulation".
#' @return A ggplot object representing the SIR model simulation plot.
#'
#' @importFrom ggplot2 "ggplot" "geom_line" "labs"
#'
#' @examples
#' # Example usage:
#' # Assuming sir_result_object@output is a data frame with columns 'time', 'S', 'I', 'R'
#' sir_result_object <- sir_result_object@output
#' sir_plot <- create_sir_plot(sir_result_object)
#' print(sir_plot)
#'
#' @export
create_sir_plot <- function(output_df, title = "SIR Model Simulation") {
  # Create the plot
  sir_plot <- ggplot(output_df, aes(x = time)) +
    geom_line(aes(y = S, colour = "Susceptible")) +
    geom_line(aes(y = I, colour = "Infected")) +
    geom_line(aes(y = R, colour = "Recovered")) +
    labs(title = title,
         x = "Time",
         y = "Proportion of Population",
         colour = "Compartment")

  # Return the plot
  return(sir_plot)
}


#Plotting function
create_sir_plot <- function(output_df, title = "SIR Model Simulation") {
  # Create the plot
  sir_plot <- ggplot(output_df, aes(x = time)) +
    geom_line(aes(y = S, colour = "Susceptible")) +
    geom_line(aes(y = I, colour = "Infected")) +
    geom_line(aes(y = R, colour = "Recovered")) +
    labs(title = title,
         x = "Time",
         y = "Proportion of Population",
         colour = "Compartment")

  # Return the plot
  return(sir_plot)
}

# Example usage
# Assuming sir_result_object@output is a data frame with columns 'time', 'S', 'I', 'R'
sir_result_object <- sir_result_object@output
sir_plot <- create_sir_plot(sir_result_object)
print(sir_plot)
