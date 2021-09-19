load("optimal_ts_data_as_005_tcrit_075.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_005_075 <- opt_ts_M
opt_ts_S_005_075 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("optimal_ts_data_as_015_tcrit_075.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_015_075 <- opt_ts_M
opt_ts_S_015_075 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("optimal_ts_data_as_005_tcrit_085.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_005_085 <- opt_ts_M
opt_ts_S_005_085 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)

load("optimal_ts_data_as_015_tcrit_085.RData")

rownames(opt_ts_M) <- h_0_values
rownames(opt_ts_S) <- h_0_values
colnames(opt_ts_M) <- b_s_values
colnames(opt_ts_S) <- b_s_values

opt_ts_M_015_085 <- opt_ts_M
opt_ts_S_015_085 <- opt_ts_S

rm(opt_ts_M, opt_ts_S, b_s_values, h_0_values)




