library(GMSE)
library(parallel)
library(foreach)
library(doParallel)
source("gmse_sims.R")

dat = gmse_sims(sims = 10, yrs = 10, 
                #save_yields = TRUE, 
                #save_budgets = TRUE, 
                #save_all = TRUE,
                RESOURCE_INI = 1000, 
                CONSUME_SURV = 2, CONSUME_REPR = 3, TIMES_FEEDING = 6, REMOVE_PR = 0,
                LAND_OWNERSHIP = TRUE, SCARING = TRUE, TEND_CROPS = TRUE, TEND_CROP_YLD = 0.2, USR_YLD_BUDGET = 0.25,
                STAKEHOLDERS = 24, USER_BUDGET = 1, OWNERSHIP_VAR = 0, 
                land_regrow = NULL)
t_length = max(unlist(lapply(dat, function(x) nrow(x$summary))))

dat = pad_summary_length(dat)

plot(1:(t_length+1), 1:(t_length+1), type = "n", ylim = c(min(unlist(lapply(dat, function(x) as.vector(x$summary[,"res"])))), 
                                      max(unlist(lapply(dat, function(x) as.vector(x$summary[,"res"])))))
     )

lapply(dat, function(x) lines(x$summary[,"res"]) )
par(new = T)
plot(1:(t_length+1), as.vector(dat[[1]]$summary[,"yield"]), col = "green", lwd = 2,type = "l", xaxt = "n", yaxt = "n",xlab = "",ylab="", 
     ylim = c(0,max(as.vector(dat[[1]]$summary[,"yield"]),na.rm=T)*1.25))

plot_each_yield = function(x) {
  z = x$yields[,2:ncol(x$yields)]
  apply(z,2,function(x) lines(x, col = "green"))
}
lapply(dat, function(x) plot_each_yield(x))
axis(4)
