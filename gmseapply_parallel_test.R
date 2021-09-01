### GMSE sims in parallel
rm(list=ls())
library(GMSE)
library(parallel)
library(foreach)
library(doParallel)

#registerDoParallel(cores=6)

cl = makeCluster(6)
registerDoParallel(cl = cl)

YRS = 10
SIMS = 50

single_sim = function() {
  
  M = list()
  
  M[[1]] = gmse_apply(get_res = "Full",
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
                      ownership_var = 0)
  
  sim_out = gmse_apply_summary(M[[1]])
  
  M[[1]]$LAND[,,2] = 1
  
  for(yr in 2:YRS) {
    M[[yr]] = gmse_apply(get_res = "Full", old_list = M[[yr-1]])
    sim_out = gmse_apply_summary(M[[yr]], output = sim_out)
    M[[yr]]$LAND[,,2] = 1  
  }
  
  sim_out
}

results <- foreach(i=1:SIMS, .export=c('gmse'), .packages='GMSE' ) %dopar% { single_sim() }
parallel::stopCluster(cl = cl)


ylo = floor(min(unlist(lapply(results, function(x) min(as.vector(x[,"res"])))))*0.9)
yhi = ceiling(max(unlist(lapply(results, function(x) max(as.vector(x[,"res"])))))*1.1)

plot(1:YRS, 1:YRS, type = "n", ylim = c(ylo,yhi))
lapply(results, function(x) lines(x[,"res"]))



