### Data processing - COMBINED OUTPUTS
source("helpers.R")

set1 = readRDS("sim_set_2/OUT.Rds")
set2 = readRDS("sim_set_3/OUT.Rds")
set3 = readRDS("sim_set_4/OUT.Rds")

get_set_pars = function(x) {
  paras = as.data.frame(NULL)
  for(i in 1:length(x)) {
    paras = rbind(paras, as.data.frame(x[[i]]$paras))
  }
  return(paras)
}

get_set_data = function(set, paras) {
  ### Calculate extinctions and length of sim "run" (i.e. number of years achieved)
  paras$EXT_PROP = NA
  paras$MEAN_TIME = NA
  paras$MEAN_R = NA
  paras$MEAN_R0 = NA
  for(i in 1:length(set)) {
    dat_i = set[[i]]$data
    
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
    paras[i, "EXT_PROP"] = sum(ext_i)/length(set)
    ### Mean duration:
    paras[i, "MEAN_TIME"] = mean(duration_i)
    
    ### Mean pop growth rates.
    ### R is N(final)-N(initial)/N(initial), R0 is the same but using zero for N(final) if the pop went extinct.
    paras[i, "MEAN_R"] = mean((nX-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
    paras[i, "MEAN_R0"] = mean((nX0-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
    
  }
  return(paras)
}

set1_par = get_set_pars(set1)
set1_par = get_set_data(set = set1, paras = set1_par)

set2_par = get_set_pars(set2)
set2_par = get_set_data(set = set2, paras = set2_par)

set3_par = get_set_pars(set3)
set3_par = get_set_data(set = set3, paras = set3_par)

set1_ext_mat = xtabs(EXT_PROP ~ OWNERSHIP_VAR + USR_BUDGET_RNG, set1_par)
filled.contour(z = set1_ext_mat, x = as.numeric(rownames(set1_ext_mat)), y = as.numeric(colnames(set1_ext_mat)),
               xlab = "Ownership variation", ylab = "Budget variation",
               nlevels = 10, col = rev(hcl.colors(10, "Reds")), 
               main = "Mean extinction probability", xaxt = "n")


meanR_matrix = xtabs(MEAN_R ~ OWNERSHIP_VAR + USR_BUDGET_RNG, set1_par)
m = meanR_matrix
filled.contour(z = m, x = as.numeric(rownames(m)), y = as.numeric(colnames(m)),
               nlevels = 10, color.palette = function(n, x) scale_cols(n, x=m), 
               main = "Mean growth rate", xaxt = "n")



plot.new()
zlo = min(set1_par$EXT_PROP,set2_par$EXT_PROP,set3_par$EXT_PROP)
zhi = max(set1_par$EXT_PROP,set2_par$EXT_PROP,set3_par$EXT_PROP)
# 0.05 + 0.25 = 0.3   (0.05,0.3)
# 0.37 + 0.25 = 0.62  (0.37,0.62)
# 0.64 + 0.25 = 0.89  (0.64, 0.89)
# 0.91 + 0.04 = 0.95  (0.91, 0.95)

par(new = "TRUE",plt = c(0.1,0.35,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set1_par$OWNERSHIP_VAR)
ycoords = unique(set1_par$USR_BUDGET_RNG)
surface.matrix = matrix(set1_par$EXT_PROP,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                col=rev(hcl.colors(20, "Reds")),
                xlab = "",ylab = "Budget variation", cex.lab = 1.5,
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi))
par(xpd = NA)
text(x=0.225,y=475,"(a) 0% yield to budget",cex = 1.5, adj =0.5)

par(new = "TRUE",plt = c(0.375,0.625,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set2_par$OWNERSHIP_VAR)
ycoords = unique(set2_par$USR_BUDGET_RNG)
surface.matrix = matrix(set2_par$EXT_PROP,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                col=rev(hcl.colors(20, "Reds")),
                xlab = "Land ownership variation",ylab = "", cex.lab = 1.5,
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi), axes = FALSE)
box()
axis(1, at = seq(range(round(xcoords,1))[1],range(round(xcoords,1))[2],0.1))
axis(2, pretty(ycoords)[1:5], labels = NA)
text(x=0.225,y=475,"(b) 25% yield to budget",cex = 1.5, adj = 0.5)

par(new = "TRUE",plt = c(0.65,0.9,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set3_par$OWNERSHIP_VAR)
ycoords = unique(set3_par$USR_BUDGET_RNG)
surface.matrix = matrix(set3_par$EXT_PROP,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                col=rev(hcl.colors(20, "Reds")),
                xlab = "",ylab = "",
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi), axes = FALSE)
box()
axis(1, at = seq(range(round(xcoords,1))[1],range(round(xcoords,1))[2],0.1))
axis(2, pretty(ycoords)[1:5], labels = NA)
text(x=0.225,y=475,"(c) 50% yield to budget",cex = 1.5, adj = 0.5)

par(new = "TRUE",plt = c(0.925,0.95,0.275,0.8),las = 1,cex.axis = 1)
filled.legend(xcoords,ycoords,surface.matrix,
              col = rev(hcl.colors(20, "Reds")),
              xlab = "",ylab = "",
              xlim = c(min(xintercepts),max(xintercepts)),
              ylim = c(min(slopes),max(slopes)),
              zlim = c(zlo,zhi))



plot.new()
zlo = min(set1_par$MEAN_R,set2_par$MEAN_R,set3_par$MEAN_R)
zhi = max(set1_par$MEAN_R,set2_par$MEAN_R,set3_par$MEAN_R)

par(new = "TRUE",plt = c(0.1,0.35,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set1_par$OWNERSHIP_VAR)
ycoords = unique(set1_par$USR_BUDGET_RNG)
surface.matrix = matrix(set1_par$MEAN_R,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                color.palette = function(n, x) scale_cols(n, x=surface.matrix),
                xlab = "",ylab = "Budget variation", cex.lab = 1.5,
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi))
par(xpd = NA)
text(x=0.225,y=475,"(a) 0% yield to budget",cex = 1.5, adj =0.5)

par(new = "TRUE",plt = c(0.375,0.625,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set2_par$OWNERSHIP_VAR)
ycoords = unique(set2_par$USR_BUDGET_RNG)
surface.matrix = matrix(set2_par$MEAN_R,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                color.palette = function(n, x) scale_cols(n, x=surface.matrix),
                xlab = "Land ownership variation",ylab = "", cex.lab = 1.5,
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi), axes = FALSE)
box()
axis(1, at = seq(range(round(xcoords,1))[1],range(round(xcoords,1))[2],0.1))
axis(2, pretty(ycoords)[1:5], labels = NA)
text(x=0.225,y=475,"(b) 25% yield to budget",cex = 1.5, adj = 0.5)

par(new = "TRUE",plt = c(0.65,0.9,0.175,0.9),las = 1,cex.axis = 1)
xcoords = unique(set3_par$OWNERSHIP_VAR)
ycoords = unique(set3_par$USR_BUDGET_RNG)
surface.matrix = matrix(set3_par$MEAN_R,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
filled.contour3(xcoords,ycoords,surface.matrix,
                color.palette = function(n, x) scale_cols(n, x=surface.matrix),
                xlab = "",ylab = "",
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(zlo,zhi), axes = FALSE)
box()
axis(1, at = seq(range(round(xcoords,1))[1],range(round(xcoords,1))[2],0.1))
axis(2, pretty(ycoords)[1:5], labels = NA)
text(x=0.225,y=475,"(c) 50% yield to budget",cex = 1.5, adj = 0.5)

par(new = "TRUE",plt = c(0.925,0.95,0.275,0.8),las = 1,cex.axis = 1)
surface.matrix = matrix(c(set1_par$MEAN_R,set2_par$MEAN_R,set2_par$MEAN_R),300,300)
xcoords = 1:300
ycoords = 1:300
filled.legend(xcoords,ycoords,surface.matrix,
              color.palette = function(n, x) scale_cols(n, x=surface.matrix),
              xlab = "",ylab = "",
              xlim = c(min(xintercepts),max(xintercepts)),
              ylim = c(min(slopes),max(slopes)),
              zlim = c(zlo,zhi))
