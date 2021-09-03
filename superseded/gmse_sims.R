### Looped gmse_apply simulations varying selected parameters and running SIMS simulations:

get_args = function(arg, default_arg) {
  missing_arg = !(names(default_arg) %in% names(arg))
  all_arg = c(arg, default_arg[names(default_arg)[missing_arg]])
  return(all_arg)
}

pad_summary_length = function(d) {
  for(i in 1:length(d)) {
    if(is.null(dim(d[[i]]$summary))) {
      d[[i]]$summary = rbind(rep(NA, length(d[[i]]$summary)),d[[i]]$summary)
      d[[i]]$summary[,"res"][1] = d[[i]]$paras$RESOURCE_INI
      d[[i]]$yields = rbind(rep(NA, length(d[[i]]$yields)), d[[i]]$yields)
    } else {
      d[[i]]$summary = rbind(rep(NA, ncol(d[[i]]$summary)), d[[i]]$summary)
      d[[i]]$summary[,"res"][1] = d[[i]]$paras$RESOURCE_INI
      d[[i]]$yields = rbind(rep(NA, length(d[[i]]$yields)), d[[i]]$yields)
    }
  }
  return(d)
}


gmse_sims = function(sims = 1, yrs = 10, save_all = FALSE, save_resXY = TRUE, save_budgets = FALSE, save_yields = FALSE,
                     RESOURCE_INI = 1000, LAND_DIM_1 = 100, LAND_DIM_2 = 100,
                  LAND_OWNERSHIP = TRUE, SCARING = TRUE, TEND_CROPS = TRUE, STAKEHOLDERS = 4, TEND_CROP_YLD = 0.2,
                  USER_BUDGET = 1000, MANAGER_BUDGET = 1000, MANAGE_TARGET = 1000,
                  USR_YLD_BUDGET = 0, USR_BUDGET_RNG = 0, OWNERSHIP_VAR = 0,
                  CONSUME_SURV = 0, CONSUME_REPR = 0, TIMES_FEEDING = 1, REMOVE_PR = 0,
                  land_regrow = NULL) {

  #stop()
#  out_all = list()
  registerDoParallel(cores=6)
  
  out_all = foreach(s = 1:sims, .export=c('gmse','gmse_apply','gmse_apply_summary'), .packages='GMSE') %dopar% {
    M = list()
    resXY = list()
    budgets_base = list()
    budgets_bonus = list()
    yields = list()
    paras = get_args(arg = as.list(match.call)[-1], default_arg = formals())
    m = try({gmse_apply(get_res = "Full", 
                        land_ownership = LAND_OWNERSHIP,
                        RESOURCE_ini = RESOURCE_INI,
                        land_dim_1 = LAND_DIM_1,
                        land_dim_2 = LAND_DIM_2,
                        ownership_var = OWNERSHIP_VAR,
                        scaring = SCARING, 
                        tend_crops = TEND_CROPS,
                        tend_crop_yld = TEND_CROP_YLD,
                        user_budget = USER_BUDGET,
                        manager_budget = MANAGER_BUDGET,
                        manage_target = MANAGE_TARGET,
                        consume_surv = CONSUME_SURV,
                        consume_repr = CONSUME_REPR,
                        remove_pr = REMOVE_PR,
                        times_feeding = TIMES_FEEDING,
                        usr_yld_budget = USR_YLD_BUDGET, 
                        stakeholders = STAKEHOLDERS, 
                        usr_budget_rng	= USR_BUDGET_RNG)}, silent = TRUE)
    if(class(m)!="list") { break }
    print(sprintf("Simulation %d/%d, step %d/%d", s, sims, 1, yrs))
    M[[1]] = m
    if(is.null(land_regrow)) {
      M[[1]]$LAND[,,2] = 1
    } else {
      if(land_regrow<=0) stop("land_regrow needs to be >0")
      land_size = dim(M[[1]]$LAND[,,2])[1]*dim(M[[1]]$LAND[,,2])[1]  
      #M[[1]]$LAND[,,2] = runif(land_size, 1*(1-land_regrow), 1*(1+land_regrow))
      M[[1]]$LAND[,,2] = rgamma(land_size, land_regrow*1000, land_regrow*1000)
    }
    
    out = gmse_apply_summary(M[[1]])
    resXY[[1]] = M[[1]]$RESOURCES[,5:6]
    budgets_base[[1]] = M[[1]]$AGENTS[,17]
    budgets_bonus[[1]] = M[[1]]$AGENTS[,26]
    yields[[1]] = M[[1]]$AGENTS[,16]
    
    i=2
    while(i <= yrs) {
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
      i = i+1
    }
    
    # if(!save_all) {
    #   out_all[[s]] = list(summary = out, paras = paras)
    #   if(save_resXY) out_all[[s]]$resXY = resXY
    #   if(save_budgets) out_all[[s]]$budgets_base = list.to.df(budgets_base)
    #   if(save_budgets) out_all[[s]]$budgets_bonus = list.to.df(budgets_bonus)
    #   if(save_yields) out_all[[s]]$yields = list.to.df(yields)
    # } else {
    #   list(all = M, summary = out, paras = paras)  
    # }
     
    #rm(M, out, budgets_base, budgets_bonus, yields, resXY)
    #out_all
    #gc()
    
    list(summary = out, paras = paras, resXY = resXY, budgets_bonus = budgets_bonus, yields = yields)
  }
  
  return(out_all)
}

### Helper function:
list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}