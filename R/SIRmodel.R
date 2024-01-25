
simulate_sir <- function(initial_susceptible, initial_infected, transmission_rate, recovery_rate, days) {
  S <- numeric(days)
  I <- numeric(days)
  R <- numeric(days)

  S[1] <- initial_susceptible
  I[1] <- initial_infected
  R[1] <- 0

  for (day in 2:days) {
    new_infected <- transmission_rate * S[day - 1] * I[day - 1]
    new_recovered <- recovery_rate * I[day - 1]

    S[day] <- S[day - 1] - new_infected
    I[day] <- I[day - 1] + new_infected - new_recovered
    R[day] <- R[day - 1] + new_recovered
  }

  return(data.frame(Day = 1:days, Susceptible = S, Infected = I, Recovered = R))
}
