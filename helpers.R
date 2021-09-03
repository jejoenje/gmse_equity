list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

### Extracts JUST the resource positions for each element of a gmse sims list,
###  i.e. returns a list of lists, of resource positions per time step per sim:
get_res_pos = function(gmse_sims_list) {
  lapply(gmse_sims_list, function(x) { lapply(x$resource, function(y) y[,c(1,5,6)]) } )
}
