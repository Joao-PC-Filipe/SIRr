

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
