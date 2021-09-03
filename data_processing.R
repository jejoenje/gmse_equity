### Data processing
source("helpers.R")
out = readRDS("OUT1.Rds")

### Reconstitute parameter values:
paras = as.data.frame(NULL)
for(i in 1:length(out)) {
  paras = rbind(paras, as.data.frame(out[[i]]$paras))
}

### Calculate extinctions and length of sim "run" (i.e. number of years achieved)
paras$EXT_PROP = NA
paras$MEAN_TIME = NA
paras$MEAN_R = NA
paras$MEAN_R0 = NA
for(i in 1:length(out)) {
  dat_i = out[[i]]$data
  
  sim_length = NULL
  n0 = paras[i, "RESOURCE_INI"] 
  time_max = paras[i, "TIME_MAX"] 
  ### All resource runs for sim i:
  res_i = lapply(dat_i, function(x) x$resources[,2])
  ### Durations for each sim:
  duration_i = unlist(lapply(res_i,length))
  ### Exinct runs in sim i:
  ext_i = duration_i!=time_max
  ### Final N for each sim:
  nX = unlist(lapply(res_i, function(x) tail(x,1)))
  nX0 = nX
  nX0[ext_i] = 0
  
  ### Proportions extinct:
  paras[i, "EXT_PROP"] = sum(ext_i)/length(out)
  ### Mean duration:
  paras[i, "MEAN_TIME"] = mean(duration_i)
  
  ### Mean pop growth rates.
  ### R is N(final)-N(initial)/N(initial), R0 is the same but using zero for N(final) if the pop went extinct.
  paras[i, "MEAN_R"] = mean((nX-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
  paras[i, "MEAN_R0"] = mean((nX0-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
  
}


par(mfrow = c(1,2))
meanE_matrix = xtabs(EXT_PROP ~ OWNERSHIP_VAR + USR_YLD_BUDGET, paras)
#meanE_matrix = meanE_matrix[1:5,1:10]
filled.contour(z = meanE_matrix, x = as.numeric(rownames(meanE_matrix)), y = as.numeric(colnames(meanE_matrix)),
              nlevels = 10, col = rev(hcl.colors(10, "Red-Yellow")), 
               main = "Mean extinction probability", xaxt = "n")
meanR_matrix = xtabs(MEAN_R ~ OWNERSHIP_VAR + USR_YLD_BUDGET, paras)
filled.contour(z = meanR_matrix, x = as.numeric(rownames(meanR_matrix)), y = as.numeric(colnames(meanR_matrix)),
               nlevels = 10, col = rev(hcl.colors(10, "RdYlBu")), 
               main = "Mean growth rate", xaxt = "n")
