
set.seed(123)

rm(list=ls())
require(MASS)
require(psych)

loading = matrix(
  c(.9, .8, .7, rep(0, 9),
    .6, .7, .8, rep(0, 9),
    .8, .9, .6),
  ncol = 3
)

correlation = matrix(
  c(1, .2, .4,
    .2, 1, .3,
    .4, .3, 1),
  ncol = 3
)

factor_structure = list(
  a = c(1, 2, 3),
  b = c(1, 2, 3),
  c = c(1, 2, 3)
)

true_correlation <- psych::sim.structure(
  fx = loading,
  Phi = correlation
)$model

n_rows = 10 # per data set
n_data_sets = 3
n_LV = nrow(correlation) # minus CV
n_meas_per_LV = 3 # 3 manifests per LV

beta0 = c(-2, 0, 2) # intercept term for control variable regression
beta1 = c(2, 0, 2)  # slope term for control variable

LV_mat = matrix(NA,
                nrow = (n_rows * n_data_sets),
                ncol = n_LV)

# add effect of a control variable
CV = rnorm(n_rows * n_data_sets,0,1)
# CV = rnorm(n_rows * n_data_sets,0,1)

for(i in 1: n_LV){
  LV_mat[,i] <- CV * beta1[i]  +
    beta0[i]
}

# sample LV conditional on CV
for(j in 1:nrow(LV_mat)){
  LV_mat[j,] = MASS::mvrnorm(
    n = 1,
    mu = LV_mat[j,] ,
    Sigma = correlation
  )
}

# add measurement error
dat = as.data.frame(LV_mat %*% t(loading)) +
  rnorm(n_meas_per_LV * n_meas_per_LV,0,1)

names(dat) <- unlist(lapply(names(factor_structure),
                               function(x) {paste(x,
                                                  factor_structure[[x]],
                                                  sep = "_")}))


split_grouping <- cut(seq(1, nrow(dat)),
                      breaks = n_data_sets,
                      labels = FALSE)

sim_data_list <- split(x = dat, f = split_grouping)

# breaks if there are more data.sets than domains
n_data_set = length( sim_data_list )
for(i in seq_along(sim_data_list)) {
  n_rows = nrow(sim_data_list[[i]])
  sim_data_list[[i]]$ID = paste0("dataset_",i,"_row_",1:n_rows)
  sim_data_list[[i]]$CV = CV[ ((i-1)*10 +1):(i*10)]
}

head(sim_data_list)

# renormalize
# visual sanity checks / understand modality


