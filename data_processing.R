### Data processing
source("helpers.R")
out = readRDS("OUT1.Rds")

paras = as.data.frame(NULL)
for(i in 1:length(out)) {
  paras = rbind(paras, as.data.frame(out[[i]]$paras))
}
