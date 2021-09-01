### GMSE sims in parallel
rm(list=ls())
library(GMSE)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=6)

YRS = 10
SIMS = 10

results <- foreach(i=1:SIMS, .export=c('gmse'), .packages='GMSE') %dopar% {
  
  gmse(time_max = YRS, 
             RESOURCE_ini = 1000, 
             consume_surv = 2, 
             consume_repr = 3, 
             times_feeding = 6, 
             remove_pr = 0.1, 
             land_ownership = TRUE, 
             scaring = TRUE, 
             tend_crops = TRUE, 
             tend_crop_yld = 0.2, 
             usr_yld_budget = 0.25, 
             stakeholders = 8, 
             user_budget = 1, 
             ownership_var = 0, plotting = FALSE)
}

### Plot resource trajectories:
# Get min/max:
yhi = ceiling(max(unlist(lapply(results, function(x) max(gmse_summary(x)$resource[,2]))))*1.1)
ylo = floor(min(unlist(lapply(results, function(x) min(gmse_summary(x)$resource[,2]))))*0.9)

par(mfrow=c(1,1))
plot(1:YRS, 1:YRS, ylim = c(ylo,yhi), type = "n")
lapply(results, function(x) lines(gmse_summary(x)$resource[,2]))



