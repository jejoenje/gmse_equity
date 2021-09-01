library(GMSE)
library(foreach)
library(doParallel)
library(plyr)

source("gmse_sims.R")


### SET 1. Varying USR_BUDGET_RNG and OWNERSHIP_VAR:
para_grid = expand.grid(OWNERSHIP_VAR = seq(0,0.9,0.1), USR_BUDGET_RNG = seq(0,900,100))

cl = parallel::makeCluster(
  6, 
  type = "PSOCK"
)
clusterEvalQ(cl,library(GMSE))
clusterExport(cl, "para_grid")

doParallel::registerDoParallel(cl = cl)
ALL = foreach(z = 1:nrow(para_grid)) %dopar% {
  #library(GMSE)
  print(sprintf("Set %d, usr_budget_rng = %1.1f, ownership_var = %1.1f...", z,  para_grid[z,"USR_BUDGET_RNG"],para_grid[z,"OWNERSHIP_VAR"]))
  return(list(
    sims = gmse_sims(USR_YLD_BUDGET = 0, STAKEHOLDERS = 12, 
                  USR_BUDGET_RNG = para_grid[z,"USR_BUDGET_RNG"], 
                  OWNERSHIP_VAR = para_grid[z,"OWNERSHIP_VAR"], sims = 100, yrs = 10, save_budgets = TRUE, save_yields = TRUE), 
    ownership_var = para_grid[z,"OWNERSHIP_VAR"],
    usr_budget_rng = para_grid[z,"USR_BUDGET_RNG"]
  ))
  
}
stopCluster(cl)

saveRDS(ALL, "set1_sims.Rds")


### SET 2. Varying OWNERSHIP_VAR and USR_YLD_BUDGET:
para_grid = expand.grid(OWNERSHIP_VAR = seq(0,0.9,0.1), USR_BUDGET_RNG = 200, USR_YLD_BUDGET = seq(0,0.9,0.1))

cl = parallel::makeCluster(
  6, 
  type = "PSOCK"
)
clusterEvalQ(cl,library(GMSE))
clusterExport(cl, "para_grid")

doParallel::registerDoParallel(cl = cl)
ALL = foreach(z = 1:nrow(para_grid)) %dopar% {
  #library(GMSE)
  print(sprintf("Set %d, usr_budget_rng = %1.1f, usr_yld_budget = %1.1f, ownership_var = %1.1f...", z,  para_grid[z,"USR_BUDGET_RNG"],para_grid[z,"USR_YLD_BUDGET"],para_grid[z,"OWNERSHIP_VAR"]))
  return(list(
    sims = gmse_sims(USR_YLD_BUDGET = para_grid[z,"USR_YLD_BUDGET"], STAKEHOLDERS = 12, 
                  USR_BUDGET_RNG = para_grid[z,"USR_BUDGET_RNG"], 
                  OWNERSHIP_VAR = para_grid[z,"OWNERSHIP_VAR"], sims = 100, yrs = 10, save_budgets = TRUE, save_yields = TRUE), 
    ownership_var = para_grid[z,"OWNERSHIP_VAR"],
    usr_budget_rng = para_grid[z,"USR_BUDGET_RNG"],
    usr_yld_budget = para_grid[z,"USR_YLD_BUDGET"]
  ))
  
}
stopCluster(cl)
saveRDS(ALL, "set2_sims.Rds")

out = as.data.frame(NULL)
for(set in 1:length(ALL)) {
  set_i = ALL[[set]]$sims
  ownership_var = ALL[[set]]$ownership_var
  usr_yld_budget = ALL[[set]]$usr_yld_budget
  for(sim in 1:length(set_i)) {
    sim_i = set_i[[sim]]
    if(class(sim_i$summary)=="numeric") {
      n_yrs = 1
      n_final = as.numeric(sim_i$summary["res"])
    } else {
      n_yrs = nrow(sim_i$summary)
      n_final = tail(sim_i$summary[,"res"],1)
    }
    out = rbind(out, c(set, sim, ownership_var, usr_yld_budget, n_yrs, n_final))
  }
}

names(out) = c("set","sim","ownership_var", "usr_yld_budget","n_yrs","n_final")

out$extinct = out$n_yrs!=10
out$meanR = (out$n_final-1000)/1000

out_summary = ddply(out, c("set","ownership_var","usr_yld_budget"), summarise, meanE = mean(extinct), meanR = mean(meanR))

par(mfrow = c(1,2))
meanE_matrix = xtabs(meanE ~ ownership_var + usr_yld_budget, out_summary)
filled.contour(meanE_matrix, nlevels = 10, col = rev(hcl.colors(10, "Red-Yellow")), 
               xlab = "Land ownership inequity", ylab = "Yield-budget bonus", main = "Mean extinction probability")

meanR_matrix = xtabs(meanR ~ ownership_var + usr_yld_budget, out_summary)
filled.contour(meanR_matrix, nlevels = 20, col = hcl.colors(20, "RdYlBu"), 
               xlab = "Land ownership inequity", ylab = "Yield-budget bonus", main = "Mean population growth rate"
)

