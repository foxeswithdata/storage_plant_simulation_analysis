rm(list=ls())

library(plotly)
library(gdata)
library(viridis)
library(rje)
library(prob)

source("code/sensitivity/matrix_extrapolate.R")
source("code/sensitivity/plotting.R")
source("code/sensitivity/data_manipulate_2.R")
source("code/foxes_pallettes.R")

path_out = "out/sensitivity_analysis"
# 
opt_ts_M_010_085_extr <- matrix_extrapolate(opt_ts_M_010_085, xby = 0.1, yby = 0.01)
opt_ts_S_010_085_extr <- matrix_extrapolate(opt_ts_S_010_085, xby = 0.1, yby = 0.01)
opt_ts_M_020_085_extr <- matrix_extrapolate(opt_ts_M_020_085, xby = 0.1, yby = 0.01)
opt_ts_S_020_085_extr <- matrix_extrapolate(opt_ts_S_020_085, xby = 0.1, yby = 0.01)
opt_ts_M_030_085_extr <- matrix_extrapolate(opt_ts_M_030_085, xby = 0.1, yby = 0.01)
opt_ts_S_030_085_extr <- matrix_extrapolate(opt_ts_S_030_085, xby = 0.1, yby = 0.01)

opt_ts_M_010_075_extr <- matrix_extrapolate(opt_ts_M_010_075, xby = 0.1, yby = 0.01)
opt_ts_S_010_075_extr <- matrix_extrapolate(opt_ts_S_010_075, xby = 0.1, yby = 0.01)

opt_ts_M_020_075_extr <- matrix_extrapolate(opt_ts_M_020_075, xby = 0.1, yby = 0.01)
opt_ts_S_020_075_extr <- matrix_extrapolate(opt_ts_S_020_075, xby = 0.1, yby = 0.01)
opt_ts_M_030_075_extr <- matrix_extrapolate(opt_ts_M_030_075, xby = 0.1, yby = 0.01)
opt_ts_S_030_075_extr <- matrix_extrapolate(opt_ts_S_030_075, xby = 0.1, yby = 0.01)

# 
# opt_ts_M_005_075_extr <- matrix_extrapolate(opt_ts_M_005_075, xby = 0.1, yby = 0.01)
# opt_ts_S_005_075_extr <- matrix_extrapolate(opt_ts_S_005_075, xby = 0.1, yby = 0.01)
# opt_ts_M_015_075_extr <- matrix_extrapolate(opt_ts_M_015_075, xby = 0.1, yby = 0.01)
# opt_ts_S_015_075_extr <- matrix_extrapolate(opt_ts_S_015_075, xby = 0.1, yby = 0.01)
# opt_ts_M_005_085_extr <- matrix_extrapolate(opt_ts_M_005_085, xby = 0.1, yby = 0.01)
# opt_ts_S_005_085_extr <- matrix_extrapolate(opt_ts_S_005_085, xby = 0.1, yby = 0.01)
# opt_ts_M_015_085_extr <- matrix_extrapolate(opt_ts_M_015_085, xby = 0.1, yby = 0.01)
# opt_ts_S_015_085_extr <- matrix_extrapolate(opt_ts_S_015_085, xby = 0.1, yby = 0.01)


# 
opt_ts_M_010_085_extr <- convert_to_df(opt_ts_M_010_085_extr)
opt_ts_S_010_085_extr <- convert_to_df(opt_ts_S_010_085_extr)
opt_ts_M_020_085_extr <- convert_to_df(opt_ts_M_020_085_extr)
opt_ts_S_020_085_extr <- convert_to_df(opt_ts_S_020_085_extr)
opt_ts_M_030_085_extr <- convert_to_df(opt_ts_M_030_085_extr)
opt_ts_S_030_085_extr <- convert_to_df(opt_ts_S_030_085_extr)
opt_ts_M_010_075_extr <- convert_to_df(opt_ts_M_010_075_extr)
opt_ts_S_010_075_extr <- convert_to_df(opt_ts_S_010_075_extr)
opt_ts_M_020_075_extr <- convert_to_df(opt_ts_M_020_075_extr)
opt_ts_S_020_075_extr <- convert_to_df(opt_ts_S_020_075_extr)
opt_ts_M_030_075_extr <- convert_to_df(opt_ts_M_030_075_extr)
opt_ts_S_030_075_extr <- convert_to_df(opt_ts_S_030_075_extr)


# opt_ts_M_005_075_extr <- convert_to_df(opt_ts_M_005_075_extr)
# opt_ts_M_015_075_extr <- convert_to_df(opt_ts_M_015_075_extr)
# opt_ts_S_005_075_extr <- convert_to_df(opt_ts_S_005_075_extr)
# opt_ts_S_015_075_extr <- convert_to_df(opt_ts_S_015_075_extr)
# opt_ts_M_005_085_extr <- convert_to_df(opt_ts_M_005_085_extr)
# opt_ts_M_015_085_extr <- convert_to_df(opt_ts_M_015_085_extr)
# opt_ts_S_005_085_extr <- convert_to_df(opt_ts_S_005_085_extr)
# opt_ts_S_015_085_extr <- convert_to_df(opt_ts_S_015_085_extr)
# # 

colnames(opt_ts_M_010_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_010_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_M_020_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_020_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_M_030_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_030_085_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_M_010_075_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_010_075_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_M_020_075_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_020_075_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_M_030_075_extr) <- c("h_0", "b_s", "ts")
colnames(opt_ts_S_030_075_extr) <- c("h_0", "b_s", "ts")

# 
# colnames(opt_ts_M_005_075_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_M_015_075_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_S_005_075_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_S_015_075_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_M_005_085_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_M_015_085_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_S_005_085_extr) <- c("h_0", "b_s", "ts")
# colnames(opt_ts_S_015_085_extr) <- c("h_0", "b_s", "ts")


opt_ts_M_010_085_extr$as <- rep(0.10, each = nrow(opt_ts_M_010_085_extr))
opt_ts_M_010_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_M_010_085_extr))
opt_ts_M_020_085_extr$as <- rep(0.20, each = nrow(opt_ts_M_020_085_extr))
opt_ts_M_020_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_M_020_085_extr))
opt_ts_M_030_085_extr$as <- rep(0.30, each = nrow(opt_ts_M_030_085_extr))
opt_ts_M_030_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_M_030_085_extr))

opt_ts_S_010_085_extr$as <- rep(0.10, each = nrow(opt_ts_S_010_085_extr))
opt_ts_S_010_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_S_010_085_extr))
opt_ts_S_020_085_extr$as <- rep(0.20, each = nrow(opt_ts_S_020_085_extr))
opt_ts_S_020_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_S_020_085_extr))
opt_ts_S_030_085_extr$as <- rep(0.30, each = nrow(opt_ts_S_030_085_extr))
opt_ts_S_030_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_S_030_085_extr))

opt_ts_M_010_075_extr$as <- rep(0.10, each = nrow(opt_ts_M_010_075_extr))
opt_ts_M_010_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_M_010_075_extr))
opt_ts_M_020_075_extr$as <- rep(0.20, each = nrow(opt_ts_M_020_075_extr))
opt_ts_M_020_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_M_020_075_extr))
opt_ts_M_030_075_extr$as <- rep(0.30, each = nrow(opt_ts_M_030_075_extr))
opt_ts_M_030_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_M_030_075_extr))

opt_ts_S_010_075_extr$as <- rep(0.10, each = nrow(opt_ts_S_010_075_extr))
opt_ts_S_010_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_S_010_075_extr))
opt_ts_S_020_075_extr$as <- rep(0.20, each = nrow(opt_ts_S_020_075_extr))
opt_ts_S_020_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_S_020_075_extr))
opt_ts_S_030_075_extr$as <- rep(0.30, each = nrow(opt_ts_S_030_075_extr))
opt_ts_S_030_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_S_030_075_extr))


# 
# 
# opt_ts_M_005_075_extr$as <- rep(0.05, each = nrow(opt_ts_M_005_075_extr))
# opt_ts_M_005_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_M_005_075_extr))
# opt_ts_M_015_075_extr$as <- rep(0.15, each = nrow(opt_ts_M_015_075_extr))
# opt_ts_M_015_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_M_015_075_extr))
# 
# opt_ts_S_005_075_extr$as <- rep(0.05, each = nrow(opt_ts_S_005_075_extr))
# opt_ts_S_005_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_S_005_075_extr))
# opt_ts_S_015_075_extr$as <- rep(0.15, each = nrow(opt_ts_S_015_075_extr))
# opt_ts_S_015_075_extr$tcrit <- rep(0.75, each = nrow(opt_ts_S_015_075_extr))
# 
# opt_ts_M_005_085_extr$as <- rep(0.05, each = nrow(opt_ts_M_005_085_extr))
# opt_ts_M_005_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_M_005_085_extr))
# opt_ts_M_015_085_extr$as <- rep(0.15, each = nrow(opt_ts_M_015_085_extr))
# opt_ts_M_015_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_M_015_085_extr))
# 
# opt_ts_S_005_085_extr$as <- rep(0.05, each = nrow(opt_ts_S_005_085_extr))
# opt_ts_S_005_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_S_005_085_extr))
# opt_ts_S_015_085_extr$as <- rep(0.15, each = nrow(opt_ts_S_015_085_extr))
# opt_ts_S_015_085_extr$tcrit <- rep(0.85, each = nrow(opt_ts_S_015_085_extr))
# # 

opt_ts_S <- data.frame()
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_010_085_extr)
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_020_085_extr)
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_030_085_extr)
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_010_075_extr)
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_020_075_extr)
opt_ts_S <- rbind(opt_ts_S, opt_ts_S_030_075_extr)

opt_ts_M <- data.frame()
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_010_085_extr)
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_020_085_extr)
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_030_085_extr)
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_010_075_extr)
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_020_075_extr)
opt_ts_M <- rbind(opt_ts_M, opt_ts_M_030_075_extr)

tcrit.labs <- c("tcrit: 0.75y", "tcrit: 0.85y")
names(tcrit.labs) <- c("0.75", "0.85")

# tcrit.labs <- c("tcrit: 0.75y")
# names(tcrit.labs) <- c("0.75")

# New facet label names for supp variable
as.labs <- c("as: 0.10kgCkg-1C", "as: 0.20kgCkg-1C", "as: 0.30kgCkg-1C")
names(as.labs) <- c("0.1", "0.2", "0.3")






p <- ggplot(opt_ts_S, aes(b_s, h_0, fill= ts)) + 
  geom_tile() +
  facet_grid( as ~ tcrit,
              labeller = labeller(as = as.labs, tcrit = tcrit.labs)) + 
  scale_y_continuous("Initial Plant Height [m]") + 
  scale_x_continuous(expression(paste("Initial Storage Mass Proportion , ", beta[s], ' , ', kgCkg^-1, C))) + 
  scale_fill_gradientn("Time Switch \nValue [d]", 
                       limits = c(0, 0.75),
                       colours = (c(foxes_palettes$dark[1], foxes_palettes$main[2], foxes_palettes$light[3]))) +
  foxes_theme
p
ggsave("opt_ts_S_new.png", plot = p, device = NULL, path = path_out,
       scale = 1, width = 140, height = 190, dpi = 600, limitsize = TRUE,
       units =  "mm")



p <- ggplot(opt_ts_M, aes(b_s, h_0, fill= ts)) + 
  geom_tile() +
  facet_grid( as ~ tcrit,
              labeller = labeller(as = as.labs, tcrit = tcrit.labs)) + 
  scale_y_continuous("Initial Plant Height [m]") + 
  scale_x_continuous(expression(paste("Initial Storage Mass Proportion , ", beta[s], ' , ', kgCkg^-1, C))) + 
  scale_fill_gradientn("Time Switch \nValue [d]", 
                       limits = c(0, 0.75),
                       colours = (c(foxes_palettes$dark[1], foxes_palettes$main[2], foxes_palettes$light[3]))) +
  foxes_theme
p

ggsave("opt_ts_M_new.png", plot = p, device = NULL, path = path_out,
       scale = 1, width = 140, height = 190, dpi = 600, limitsize = TRUE,
       units =  "mm")





