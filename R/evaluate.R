
evaluate = function(template){
  data(data2024,envir=environment(),package="G2F")
  PSet = template
  names(VSet)[3] = 'TBV'
  Val = merge(PSet,VSet,by = c('Env','Hybrid'))
  pa = by(Val,Val$Env,function(x) cor(x$Yield_Mg_ha,x$TBV,use='p'))
  return(mean(pa))
}

