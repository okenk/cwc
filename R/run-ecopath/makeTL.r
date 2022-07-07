makeTL=function (DC,Grouptype)     ## changed p to DC
{
index=which(Grouptype==2)
index2=which(Grouptype<2)
ngroups=length(Grouptype)
TL=matrix(1,ngroups,1)

# Creat matrix
n.consumers=length(index)
A=matrix(0,n.consumers,n.consumers)
G=matrix(0,n.consumers,1)
for (i in 1:n.consumers)
{
    G[i,1]=-2*sum(DC[index2,index[i]])-sum(DC[index,index[i]])         ## changed p to DC
    A[i,1:n.consumers]=t(DC[index,index[i]])                  ## changed p to DC
    A[i,i]=A[i,i]-1
}

TLshort=solve(A)%*%G
TL[index]=TLshort
return (TL)
}
