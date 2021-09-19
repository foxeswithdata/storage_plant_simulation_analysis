load("data/sensitivity_data/december/optimal_ts_data_as_0.1_tcrit_0.85.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_010_085 <- opt_ts_M
opt_ts_S_010_085 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("data/sensitivity_data/december/optimal_ts_data_as_0.2_tcrit_0.85.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_020_085 <- opt_ts_M
opt_ts_S_020_085 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)


load("data/sensitivity_data/december/optimal_ts_data_as_0.3_tcrit_0.85.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_030_085 <- opt_ts_M
opt_ts_S_030_085 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("data/sensitivity_data/december/optimal_ts_data_as_0.3_tcrit_0.75.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_030_075 <- opt_ts_M
opt_ts_S_030_075 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("data/sensitivity_data/december/optimal_ts_data_as_0.2_tcrit_0.75.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_020_075 <- opt_ts_M
opt_ts_S_020_075 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("data/sensitivity_data/december/optimal_ts_data_as_0.1_tcrit_0.75.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_010_075 <- opt_ts_M
opt_ts_S_010_075 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)