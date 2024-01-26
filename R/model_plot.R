Model_sample %>% ggplot(aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 0.75) +
  geom_line(aes(y = I, color = "Infectious"), linewidth = 0.75) +
  geom_line(aes(y = R, color = "Recovered"), linewidth = 0.75) +
  labs(title = "SIR Model Dynamics t",
       x = "Time",
       y = "Proportion of Population") +
  scale_color_manual(values = c("Susceptible" = "blue", "Infectious" = "red", "Recovered" = "lightgreen"))+
  theme_minimal()

##### The graph isn't useful because the number of cases is a small fraction
## in relation to the population of Italy


###Netherlands might be more suitable:
View(test)
test <- WHO_COVID_19_global_data %>% filter(Country == "Netherlands")%>%
  select(Date = Date_reported,New_daily_cases = New_cases,Cumulative_cases) %>%
  mutate(day = 1:n())
test %>% filter(New_daily_cases!=0) %>% filter(650<day & day<850) %>% ggplot(aes(day,New_daily_cases)) +geom_line()
test %>% ggplot(aes(day,Cumulative_cases/17000000))+geom_line()
