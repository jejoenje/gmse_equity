### GMSE sims in parallel
rm(list=ls())
library(GMSE)
library(parallel)
library(doParallel)
library(foreach)
source("helpers.R")

foldername = "sim_set_2a"

YRS = 20
SIMS = 100

par(mfrow=c(1,1))

OUT = list()

#PARS = expand.grid(STAKEHOLDERS = 12, USR_YLD_BUDGET = seq(0,0.45,0.05), OWNERSHIP_VAR = seq(0.1,1,0.1))
PARS = expand.grid(TIME_MAX = YRS,
                   RESOURCE_INI = 1000,
                   CONSUME_SURV = 2,
                   CONSUME_REPR = 3,
                   TIMES_FEEDING = 6,
                   REMOVE_PR = 0.1,
                   LAND_OWNERSHIP = TRUE,
                   SCARING = TRUE,
                   TEND_CROPS = TRUE,
                   TEND_CROP_YLD = 0.2,
                   USR_YLD_BUDGET = 0,
                   STAKEHOLDERS = 12, 
                   USER_BUDGET = 1000,
                   USR_BUDGET_RNG = seq(0,450,50),
                   OWNERSHIP_VAR = seq(0.5,0.7,0.05)
                   )

for(k in 1:nrow(PARS)) {
  #cat(sprintf("\n"), file = "foreach_log.txt", append = TRUE)
  cat(sprintf("\n%s - parameter set %d / %s.. \n\n", Sys.time(), k, nrow(PARS)), file = paste0(foldername,"/foreach_log.txt"), append = TRUE)
  cat(sprintf("\n%s - parameter set %d / %s..", Sys.time(), k, nrow(PARS)))
  
  cl = makeCluster(6,outfile="foreach_log.txt")
  registerDoParallel(cl = cl)

  results <- foreach(i=1:SIMS, .export=c('gmse'), .packages=c('GMSE')) %dopar% {
    gmse(time_max = PARS$TIME_MAX[k], 
         RESOURCE_ini = PARS$RESOURCE_INI[k], 
         consume_surv = PARS$CONSUME_SURV[k], 
         consume_repr = PARS$CONSUME_REPR[k], 
         times_feeding = PARS$TIMES_FEEDING[k], 
         remove_pr = PARS$REMOVE_PR[k], 
         land_ownership = PARS$LAND_OWNERSHIP[k], 
         scaring = PARS$SCARING[k], 
         tend_crops = PARS$TEND_CROPS[k], 
         tend_crop_yld = PARS$TEND_CROP_YLD[k], 
         usr_yld_budget = PARS$USR_YLD_BUDGET[k], 
         stakeholders = PARS$STAKEHOLDERS[k], 
         user_budget = PARS$USER_BUDGET[k], 
         usr_budget_rng = PARS$USR_BUDGET_RNG[k],
         ownership_var = PARS$OWNERSHIP_VAR[k], 
         plotting = FALSE)
  }
  
  ### Get results summary:  
  res_summary = list(data = lapply(results, gmse_summary))
  ### Add resource positions to summary:
  # res_positions = get_res_pos(results)
  # for(z in 1:length(res_summary$data)) {
  #   res_summary$data[[z]]$res_positions = res_positions[[z]]
  # }
  
  ### Get and save parameters with output list:
  pars = as.list(as.matrix(PARS[k,]))
  names(pars) = names(PARS)
  res_summary$paras = pars
  
  #rm(results, res_positions)
  rm(results)
  
  OUT[[k]] = res_summary
  
  gc()
  
  stopCluster(cl)
  
}

saveRDS(OUT, paste0(foldername,"/OUT.Rds"))




