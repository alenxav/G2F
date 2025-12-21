# MegaSEM
require(bWGR) 
data(data2024)
Z = IMP(Z)

# Minor QC: drop locs not part of the TPE
dta$Loc = factor(substr(dta$Env,1,2))
PSet$Loc = substr(PSet$Env,1,2)
dta = subset(dta, Loc %in% unique(PSet$Loc))

# Reparameterization: Gaussian kernel
X = K2X(EigenGAU(Z,cores=-1,phi=0.75)) # 0.4531198
rownames(X) = rownames(Z)

# Change data from tall to wide
Y = reshape2::acast(dta, Hybrid~Env, mean, value.var = 'Yield_Mg_ha')
Y = Y[,which(colSums(!is.na(Y))>100)] |> apply(2,function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T) )
overlap = intersect(rownames(Y),rownames(X))
Y = Y[overlap,]

# Fit MegaSEM model
fit = XSEMF(Y, X[overlap,], npc = 10)
hat0 = X %*% fit$b

# Fit something on the residuals (extra juice) # 0.4533147
Y2 = Y-hat0[overlap,]
fit2 = apply(Y2,2,function(y) MRR3F(as.matrix(y[which(!is.na(y))]), (Z[overlap,])[which(!is.na(y)),],NonLinearFactor=3)$b )
hat00 = hat0 + Z %*% fit2*0.2

# State selection index
states = substr(colnames(Y),1,2)
hat = t(apply(hat00,1,function(x)tapply(x,states,mean)))
colnames(hat) = gsub('Loc','',colnames(hat))

# Pool predictions by similarity
SI = cutree(hclust(as.dist(1-cor(hat)),'ward.D2'),10)
hat2 = t(apply(hat,1,function(x)tapply(x,SI,mean)))[,SI] |> apply(2,function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T) )
colnames(hat2) = colnames(hat)

# Validation set
PSet$Loc = substr(PSet$Env,1,2)
for(i in 1:nrow(PSet)){  PSet$Yield_Mg_ha[i] = hat2[ PSet$Hybrid[i], PSet$Loc[i]] }
PSet = PSet[,1:3]
evaluate(PSet)
