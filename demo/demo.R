ct=matrix(c(2000,0,1,2000),2,2)
kappa.ct(ct)
ct
set=cbind(
  c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
  c(1,1,1,1,1,1,0,0,0,1,1,1,1,0,0,0,0)
)
kappa(set)
kappa.ct(set)
kappa(ct)
set2=cbind(
  c(3,6,2,4,5,3,5,6,3,4,6,4,3,4,6,4,3),
  c(7,5,6,5,4,6,4,4,6,4,4,6,4,5,6,4,4)
)
kappa(set2)
kappa.ct(set2)

x=rho.power(.1,inc = 10)
x
rho.set(set)

rho.ct(ct)
rho.ct(contingencyTable)
