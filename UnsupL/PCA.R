states = row.names(USArrests)

states

names(USArrests)

#Check means (col wise)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

#Need to scale before doing PCA, otherwise will be driven by Assault. 
pr.out = prcomp(USArrests, scale = T)
names(pr.out)

#Means and sds that were used to scale variables before PCA. 
pr.out$center
pr.out$scale

#PC loading vectors
pr.out$rotation

dim(pr.out$x)
biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
 

pr.var=pr.out$sdev ^2
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type="b")
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")
 