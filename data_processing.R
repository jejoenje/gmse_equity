### Data processing

out = readRDS("OUT1.Rds")

list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

paras = as.data.frame(NULL)
for(i in 1:length(out)) {
  paras = rbind(paras, as.data.frame(out[[i]]$paras))
}
