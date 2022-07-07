#function [vmat, amat]=makev(Mpmat,Bmat,vuln,Grouptype)
makev=function(Mpmat,Bmat,vuln,Grouptype)
{
warning off MATLAB:divideByZero        ###??? supresses warning when divide by zero - how translate?
ngroups=size[Mpmat,1]
vmat=rep(0,ngroups, ngroups)
amat=rep(0,ngroups, ngroups)
for (i in 1:ngroups) # cycle through consumers
 {
        if (Grouptype[i]>1)
        {
        P=Bmat[i]
        Vstarlist=vuln[,i]
        Mpstarlist=Mpmat[,i]
        vmat[,i]=Vstarlist*Mpstarlist
        amat[,i]=2*vmat[,i]*Mpstarlist/(P*vmat[,i]-P*Mpstarlist)
        }
}

index=find(isnan[amat]==1)
amat[index]=seq(0,length(index),1)
res=list(vmat=vmat, amat=amat)
return (res)
}


