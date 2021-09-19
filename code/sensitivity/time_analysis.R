rm(list=ls())

library(ggplot2)

load("time_est_single_species.RData")

runs$total_time[runs$sim_length > 75] = runs$total_time[runs$sim_length > 75] * 60

p <- ggplot(runs, aes(x = sim_length, y = total_time)) + 
  geom_point() + 
  scale_y_continuous("Simulation Time [s]") +
  scale_x_continuous("Length Simulated [yr]")
p

any(is.nan(runs$total_time ))


p <- ggplot(runs, aes(x = total_time)) + 
  geom_histogram()
p
