library(GMSE)

rm(list=ls())



SIMS = 10
YRS = 10

### All output combined:
out_all = list()

for(s in 1:SIMS) {
  
  ### For individual sim runs:
  yld = as.data.frame(NULL)
  bud = as.data.frame(NULL)
  res_pos = list()
  all = list()
  
  m = try({gmse_apply(get_res = "Full", land_ownership = TRUE, scaring = TRUE, tend_crops = TRUE, ownership_var = 0, stakeholders = 12, usr_yld_budget = 0.5)}, silent = T)
  
  if(class(m)!="list") break
  
  b = m$AGENTS[2:nrow(m$AGENTS),17]
  b = rpois(b, b)
  m$AGENTS[2:nrow(m$AGENTS),17] = b
  # [1]  990 1032  980 1039
  out = gmse_apply_summary(m)
  res_pos[[1]] = m$RESOURCES[,5:6]
  all[[1]] = m
  
  yld = rbind(yld, tapply(m$LAND[,,2], m$LAND[,,3], sum))
  bud = rbind(bud, b)
  
  for(i in 2:YRS) {
    m2 = try({gmse_apply(get_res = "Full", old_list = m)},silent =T)
    
    if(class(m2)!="list") {
      break
    } else {
      out = gmse_apply_summary(m2, output = out)
      yld = rbind(yld, tapply(m2$LAND[,,2], m2$LAND[,,3], sum))
      bud = rbind(bud, b)
      res_pos[[i]] = m2$RESOURCES[,5:6]
      all[[i]] = m2
      m = m2    
    }
  }
  
  names(yld) = paste0("y_",1:(nrow(m$AGENTS)-1))
  names(bud) = paste0("b_",1:(nrow(m$AGENTS)-1))
  
  if(is.null(dim(out))) {
    out = c(out, yld, bud)
  } else {
    #bud = rbind(rep(1000, m$stakeholders),bud)
    out = cbind(out, yld, bud)  
  }
  
  # if(nrow(out)!=YRS) {
  #   pad_out = matrix(NA, ncol = ncol(out), nrow = YRS-nrow(out))
  #   pad_out = as.data.frame(pad_out)
  #   names(pad_out) = names(out)
  #   out = rbind(out, pad_out)
  # }
  out = list(summary = out, res_pos = res_pos, all = all)
  
  out_all[[s]] = out
  rm(dat,yld,res_pos,out,b,m,m2)
  
}



lo = floor(min(unlist((lapply(out_all, function(x) min(x$summary$res)))))*0.95)
hi = ceiling(max(unlist((lapply(out_all, function(x) max(x$summary$res)))))*1.05)

yrs_complete = unlist(lapply(out_all, function(x) {
  if(is.null(nrow(x$summary))) {
    return(1)
  } else {
    return(nrow(x$summary))
  }
}))

line_cols = rep("black", YRS)
line_cols[yrs_complete<YRS] = "red"
plot(1:YRS, sample(seq(lo, hi),10), type = "n", ylim = c(lo,hi), xlab = "Year", ylab = "Population size")
for(i in 1:length(out_all)) {
  lines(out_all[[i]]$summary$res, col = line_cols[i])
}

final_N = unlist(lapply(out_all, function(x) tail(x$summary$res,1)))
final_N[yrs_complete<YRS] = 0
mean((final_N-1000)/1000)


# par(mfrow=c(1,2))
# N = 1000
# z = rnbinom(N, mu = 10, size = 20)
# p = rpois(N, 10)
# 
# hist(p); mean(p); var(p)
# hist(z); mean(z); var(z)
# 



###
###
### TESTING CODE
###
###

M = list()
m = gmse_apply(get_res = "Full", land_ownership = TRUE, scaring = TRUE, tend_crops = TRUE, usr_yld_budget = 0.4, stakeholders = 5, 
               usr_budget_rng	= 1000*0)
M[[1]] = m
#M[[1]]$AGENTS[2:(M[[1]]$stakeholders+1),17] = M[[1]]$AGENTS[2:(M[[1]]$stakeholders+1),17] + M[[1]]$AGENTS[2:(M[[1]]$stakeholders+1),26]
M[[1]]$LAND[,,2] = 1
out = gmse_apply_summary(M[[1]])

for(i in 2:10) {
  print(i)
  M[[i]] = gmse_apply(get_res = "Full", old_list = M[[i-1]])
  #M[[i]]$AGENTS[2:(M[[i]]$stakeholders+1),17] = M[[i]]$AGENTS[2:(M[[i]]$stakeholders+1),17] + M[[i]]$AGENTS[2:(M[[i]]$stakeholders+1),26]
  M[[i]]$LAND[,,2] = 1
  out = gmse_apply_summary(M[[i]], output = out)
}
#list.to.df(lapply(M, function(x) x$AGENTS[,17]))
out
sum(out[,c("culls","scares","tend_crops")])





library(GMSE)
rm(list=ls())

list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}
mysims = function(sims = 1, yrs = 10, save_all = FALSE, save_resXY = TRUE, save_budgets = FALSE, save_yields = FALSE,
                  LAND_OWNERSHIP = TRUE, SCARING = TRUE, TEND_CROPS = TRUE, STAKEHOLDERS, USR_YLD_BUDGET, USR_BUDGET_RNG, OWNERSHIP_VAR) {
  
  out_all = list()
  for(s in 1:sims) {
    M = list()
    resXY = list()
    budgets_base = list()
    budgets_bonus = list()
    yields = list()
    m = try({gmse_apply(get_res = "Full", 
                   land_ownership = LAND_OWNERSHIP,
                   ownership_var = OWNERSHIP_VAR,
                   scaring = SCARING, 
                   tend_crops = TEND_CROPS, 
                   usr_yld_budget = USR_YLD_BUDGET, 
                   stakeholders = STAKEHOLDERS, 
                   usr_budget_rng	= USR_BUDGET_RNG)}, silent = TRUE)
    if(class(m)!="list") { break }
    print(sprintf("Simulation %d/%d, step %d/%d", s, sims, 1, yrs))
    M[[1]] = m
    M[[1]]$LAND[,,2] = 1
    out = gmse_apply_summary(M[[1]])
    resXY[[1]] = M[[1]]$RESOURCES[,5:6]
    budgets_base[[1]] = M[[1]]$AGENTS[,17]
    budgets_bonus[[1]] = M[[1]]$AGENTS[,26]
    yields[[1]] = M[[1]]$AGENTS[,16]
      
    for(i in 2:yrs) {
      print(sprintf("Simulation %d/%d, step %d/%d", s, sims, i, yrs))
      m_new = try({gmse_apply(get_res = "Full", old_list = M[[i-1]])}, silent = T)
      if(class(m_new)!="list") break
      M[[i]] = m_new
      M[[i]]$LAND[,,2] = 1
      out = gmse_apply_summary(M[[i]], output = out)
      resXY[[i]] = M[[i]]$RESOURCES[,5:6]
      budgets_base[[i]] = M[[i]]$AGENTS[,17]
      budgets_bonus[[i]] = M[[i]]$AGENTS[,26]
      yields[[i]] = M[[i]]$AGENTS[,16]
    }
    
    if(!save_all) {
      out_all[[s]] = list(summary = out)
      if(save_resXY) out_all[[s]]$resXY = resXY
      if(save_budgets) out_all[[s]]$budgets_base = list.to.df(budgets_base)
      if(save_budgets) out_all[[s]]$budgets_bonus = list.to.df(budgets_bonus)
      if(save_yields) out_all[[s]]$yields = list.to.df(yields)
    } else {
      out_all[[s]] = list(all = M, summary = out)  
    }
    
    rm(M, out, budgets_base, budgets_bonus, yields, resXY)
    gc()
  }

  return(out_all)
}

# test = mysims(USR_YLD_BUDGET = 0, STAKEHOLDERS = 8, USR_BUDGET_RNG = 900, OWNERSHIP_VAR = 0, sims = 1, yrs = 10, save_budgets = TRUE, save_yields = TRUE)
# 
# format(object.size(test), "Kb")
# test[[1]]$budgets_base
# test[[1]]$budgets_bonus
# test[[1]]$yields

para_grid = expand.grid(OWNERSHIP_VAR = seq(0,0.9,0.1), USR_BUDGET_RNG = seq(0,900,100))
ALL = list()
for(z in 1:nrow(para_grid)) {
  print(sprintf("Set %d, usr_budget_rng = %1.1f, ownership_var = %1.1f...", z,  para_grid[z,"USR_BUDGET_RNG"],para_grid[z,"OWNERSHIP_VAR"]))
  ALL[[z]] = mysims(USR_YLD_BUDGET = 0, STAKEHOLDERS = 12, 
                  USR_BUDGET_RNG = para_grid[z,"USR_BUDGET_RNG"], 
                  OWNERSHIP_VAR = para_grid[z,"OWNERSHIP_VAR"], sims = 10, yrs = 10, save_budgets = TRUE, save_yields = TRUE)
}

#ALL = list()
library(foreach)
library(doParallel)
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
    sims = mysims(USR_YLD_BUDGET = 0, STAKEHOLDERS = 12, 
                  USR_BUDGET_RNG = para_grid[z,"USR_BUDGET_RNG"], 
                  OWNERSHIP_VAR = para_grid[z,"OWNERSHIP_VAR"], sims = 100, yrs = 10, save_budgets = TRUE, save_yields = TRUE), 
    ownership_var = para_grid[z,"OWNERSHIP_VAR"],
    usr_budget_rng = para_grid[z,"USR_BUDGET_RNG"]
  ))
           
}
stopCluster(cl)

stepsToExtinction = function(x) {
  if(class(x)=="numeric") { return(1) } else {
    return(nrow(x))
  }
}

ALL_summary = as.data.frame(NULL)
for(set in 1:nrow(para_grid)) {
  set_i = ALL[[set]]
  for(sim in 1:length(set_i)) {
    print(sprintf("Processing set %d, sim %d...", set, sim))
    set_sim = set_i$sims[[sim]]
    set_sim_ownership_var = set_i$ownership_var
    set_sim_usr_budget_rng = set_i$usr_budget_rng
    if(class(set_sim$summary)=="numeric") {
      tte = 1
      N_final = set_sim$summary["res"]
    } else {
      tte = nrow(set_sim$summary)
      N_final = tail(set_sim$summary[,"res"],1)
    }
    all_summary_i = para_grid[set,]
    all_summary_i$ownership_var = set_sim_ownership_var
    all_summary_i$usr_budget_rng = set_sim_usr_budget_rng
    all_summary_i$tte = tte
    all_summary_i$N_final = N_final
    ALL_summary = rbind(ALL_summary, all_summary_i)
  }
}
ALL_summary$N_trend = (ALL_summary$N_final-1000)/1000

getMeanExtProb = function(x) {
  sum(x!=10)/length(x)
}

library(plyr)
mean_growth = ddply(ALL_summary, c("OWNERSHIP_VAR","USR_BUDGET_RNG"), summarise, meanR = mean(N_trend))
mean_growth$USR_BUDGET_RNG = mean_growth$USR_BUDGET_RNG/1000
mean_growth_matrix = xtabs(meanR ~ OWNERSHIP_VAR + USR_BUDGET_RNG, mean_growth)
filled.contour(mean_growth_matrix, nlevels = 10, col = hcl.colors(10, "RdYlBu"))

mean_ext = ddply(ALL_summary, c("OWNERSHIP_VAR","USR_BUDGET_RNG"), summarise, meanEXTP = getMeanExtProb(tte))
mean_ext$USR_BUDGET_RNG = mean_ext$USR_BUDGET_RNG/1000
mean_ext_matrix = xtabs(meanEXTP ~ OWNERSHIP_VAR + USR_BUDGET_RNG, mean_ext)
filled.contour(mean_ext_matrix, nlevels = 10, col = rev(hcl.colors(10, "Red-Yellow")), yaxt = "n")



count_yrs = function(x) {
  if(class(x)=="numeric") { return(1) } else { return(nrow(x))}
}

out = as.data.frame(NULL)
for(set in 1:length(ALL)) {
  set_i = ALL[[set]]$sims
  ownership_var = ALL[[set]]$ownership_var
  usr_budget_rng = ALL[[set]]$usr_budget_rng
  for(sim in 1:length(set_i)) {
    sim_i = set_i[[sim]]
    if(class(sim_i$summary)=="numeric") {
      n_yrs = 1
      n_final = as.numeric(sim_i$summary["res"])
    } else {
      n_yrs = nrow(sim_i$summary)
      n_final = tail(sim_i$summary[,"res"],1)
    }
    out = rbind(out, c(set, sim, ownership_var, usr_budget_rng, n_yrs, n_final))
  }
}
names(out) = c("set","sim","ownership_var", "usr_budget_rng","n_yrs","n_final")

out$extinct = out$n_yrs!=10
out$meanR = (out$n_final-1000)/1000

out_summary = ddply(out, c("set","ownership_var","usr_budget_rng"), summarise, meanE = mean(extinct), meanR = mean(meanR))

par(mfrow = c(1,2))
meanE_matrix = xtabs(meanE ~ ownership_var + usr_budget_rng, out_summary)
filled.contour(meanE_matrix, nlevels = 8, col = rev(hcl.colors(8, "Red-Yellow")), 
               xlab = "Land ownership inequity", ylab = "Budget inequity", main = "Mean extinction probability")

meanR_matrix = xtabs(meanR ~ ownership_var + usr_budget_rng, out_summary)
filled.contour(meanR_matrix, nlevels = 10, col = hcl.colors(10, "RdYlBu"), 
               xlab = "Land ownership inequity", ylab = "Budget inequity", main = "Mean population growth rate"
               )



test = mysims(sims = 2, USR_YLD_BUDGET = 0.1, STAKEHOLDERS = 4, USR_BUDGET_RNG = 0)
format(object.size(test), "Kb")

test = mysims(sims = 2, save_resXY = TRUE, USR_YLD_BUDGET = 0.1, STAKEHOLDERS = 4, USR_BUDGET_RNG = 0)
format(object.size(test), "Kb")



test = mysims(SIMS = 2, SAVE_ALL = TRUE, USR_YLD_BUDGET = 0.1, STAKEHOLDERS = 4, USR_BUDGET_RNG = 0)
format(object.size(test), "Kb")
