mean_stress = 0.85
std_stress = 60/365

mean_stress2 = 0.85
std_stress2 = 30/365

mean_stress3 = 0.85
std_stress3 = 15/365

mean_stress4 = 0.85
std_stress4 = 0/365

mean_stress5 = 0.75
std_stress5 = 60/365

mean_stress6 = 0.75
std_stress6 = 30/365

mean_stress7 = 0.75
std_stress7 = 15/365

mean_stress8 = 0.75
std_stress8 = 0

size = 1000000

realised_stress = rnorm(size, mean = mean_stress, sd = std_stress)
realised_stress2 = rnorm(size, mean = mean_stress2, sd = std_stress2)
realised_stress3 = rnorm(size, mean = mean_stress3, sd = std_stress3)
realised_stress4 = rnorm(size, mean = mean_stress4, sd = std_stress4)
realised_stress5 = rnorm(size, mean = mean_stress5, sd = std_stress5)
realised_stress6 = rnorm(size, mean = mean_stress6, sd = std_stress6)
realised_stress7 = rnorm(size, mean = mean_stress7, sd = std_stress7)
realised_stress8 = rnorm(size, mean = mean_stress8, sd = std_stress8)


# real_stress = data.frame(stress = c(realised_stress, realised_stress_2),
#                          mean = rep(c(mean_stress, mean_stress_2), each = size),
#                          std = rep(c(std_stress, std_stress_2), each = size),
#                          env = rep(c("MD-HS", "HD-LS"), each = size))

real_stress_full = data.frame(stress = c(realised_stress, realised_stress2, realised_stress3,
                                         realised_stress5, realised_stress6, realised_stress7),
                         mean = rep(c(mean_stress, mean_stress2,mean_stress3,
                                      mean_stress5, mean_stress6, mean_stress7), each = size),
                         std = rep(c(60, 30, 15,
                                     60, 30, 15), each = size),
                         env = rep(c("LD-HS", "LD-MS","LD-LS",
                                     "MD-HS", "MD-MS","MD-LS"), each = size))


# real_stress$stress_dur = (1 - real_stress$stress) * 365
real_stress_full$stress_dur = (1 - real_stress_full$stress) * 365


real_stress_full = data.frame(stress = c(realised_stress2,
                                         realised_stress7),
                              mean = rep(c(mean_stress2,
                                           mean_stress7), each = size),
                              std = rep(c(30,
                                          15), each = size),
                              env = rep(c("LD-MS",
                                          "MD-LS"), each = size))
real_stress_full$stress_dur = (1 - real_stress_full$stress) * 365


p <- ggplot(real_stress_full, aes(x = stress_dur, color = env)) + 
  geom_density() + 
  scale_color_manual("Environment", values = c(foxes_palettes$main[2], foxes_palettes$main[1]),
                     labels = c("Med Stoch - Low Stress", "Low Stoch - Med Stress")) + 
  scale_x_continuous("Duration of Stress (d)") +
  scale_y_continuous("Probability Density") + 
  coord_cartesian(xlim = c(0, 365)) + 
  foxes_theme

p



file_figure <- tempfile(paste("environment", "stress", "duration", "all", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")





p <- ggplot(real_stress_full, aes(x = stress_dur, color = as.factor(mean), linetype = as.factor(std))) + 
  geom_density() + 
  scale_color_manual("Stress Duration", values = c(foxes_palettes$main[1], foxes_palettes$main[2]),
                     labels = c("Low Stress", "Medium Stress")) + 
  scale_linetype_discrete("Stochasticity",
                     labels = c("Low", "Medium", "High")) + 
  scale_x_continuous("Duration of Stress (d)") +
  scale_y_continuous("Density") + 
  coord_cartesian(xlim = c(0, 365)) + 
  facet_grid(std~ mean, labeller = labeller(mean = c("0.75" = "Med Stress",
                                                                "0.85" = "Low Stress",
                                                                "1" = "No Stress"),
                                            std = c("15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme

p



file_figure <- tempfile(paste("environment", "stress", "duration", "all", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

