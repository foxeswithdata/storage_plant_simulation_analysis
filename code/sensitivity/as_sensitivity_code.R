rm(list=ls())

load("data/sensitivity_data/december/as_sensitivity.RData")

library(ggplot2)
as_sens <- out

any(out$died)


tcrit.labs <- c("tcrit: 0.75y ", "tcrit: 0.85 y")
names(tcrit.labs) <- c("0.75", "0.85")

# New facet label names for supp variable
ts.labs <- c("ts: 0.25 y", "ts: 0.33 y", "ts: 0.50 y", "ts: 0.66 y")
names(ts.labs) <- c("0.25", "0.33", "0.5", "0.66")


p <- ggplot(as_sens, aes(x = as, y = RGR_H, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  geom_vline(xintercept = 0.1, linetype = "dashed", alpha = 0.75, color = "#083855") +
  geom_vline(xintercept = 0.3, linetype = "dashed", alpha = 0.75, color = "#083855") +
  scale_x_continuous(expression(paste("Storage Utilisation Rate ", alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste("Relative Height Growth ", RI[h]))) +
  scale_color_manual("Time Switch \nValue (d)", 
                       values = rev(foxes_strategy_gradient_2(6))) +
  foxes_theme
p

ggsave("RGR_height.png", plot = p, device = NULL, path = path_out,
       scale = 1, width = 140, height = 190, dpi = 600, limitsize = TRUE,
       units =  "mm")


p <- ggplot(as_sens, aes(x = as, y = RGR_CST, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  geom_vline(xintercept = 0.1, linetype = "dashed", alpha = 0.75, color = "#083855") +
  geom_vline(xintercept = 0.3, linetype = "dashed", alpha = 0.75, color = "#083855") +
  scale_x_continuous(expression(paste("Storage Utilisation Rate ", alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste("Relative Storage Concentration Change ", RI[cst]))) +
  scale_color_manual("Time Switch \nValue (d)", 
                     values = rev(foxes_strategy_gradient_2(6))) +
  foxes_theme
p

ggsave("RGR_storage.png", plot = p, device = NULL, path = path_out,
       scale = 1, width = 140, height = 190, dpi = 600, limitsize = TRUE,
       units =  "mm")


















p <- ggplot(as_sens, aes(x = as, y = DELTA_H, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous("Height Increase [m]") +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_delta_h.png", width = 1000, height = 800, res = 200)
p
dev.off()

p <- ggplot(as_sens, aes(x = as, y = RGR_LA, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste(RI[la]))) +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_rgr_la.png", width = 1000, height = 800, res = 200)
p
dev.off()

p <- ggplot(as_sens, aes(x = as, y = DELTA_LA, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous("Leaf Area Increase [m2]") +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_delta_la.png", width = 1000, height = 800, res = 200)
p
dev.off()


p <- ggplot(as_sens, aes(x = as, y = RGR_MA, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste(RI[ma]))) +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_rgr_ma.png", width = 1000, height = 800, res = 200)
p
dev.off()

p <- ggplot(as_sens, aes(x = as, y = DELTA_MA, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous("Live Mass Increase [kgC]") +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_delta_ma.png", width = 1000, height = 800, res = 200)
p
dev.off()




p <- ggplot(as_sens, aes(x = as, y = RGR_MST, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste(RI[mst]))) +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_rgr_mst.png", width = 1000, height = 800, res = 200)
p
dev.off()



p <- ggplot(as_sens, aes(x = as, y = DELTA_MST, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous("Storage Mass Increase [kgC]") +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_delta_mst.png", width = 1000, height = 800, res = 200)
p
dev.off()



p <- ggplot(as_sens, aes(x = as, y = RGR_CST, color = as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous(expression(paste(RI[cst]))) +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p

png(filename="a_s_rgr_cst.png", width = 1000, height = 800, res = 200)
p
dev.off()


p <- ggplot(as_sens, aes(x = as, y = DELTA_CST, color= as.factor(height_0))) +
  geom_point() + 
  facet_grid( ts ~ tcrit, 
              labeller = labeller(ts = ts.labs, tcrit = tcrit.labs)) + 
  scale_x_continuous(expression(paste(alpha[s], ' ', kgCkgC^-1, d^-1))) +
  scale_y_continuous("Storage Concentration Increase []") +
  scale_color_discrete(name = "Initial Height [m]") +
  theme_bw()
p


png(filename="a_s_delta_cst.png", width = 1000, height = 800, res = 200)
p
dev.off()


