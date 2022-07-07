ecopathinput=read.csv("CNPTestmodel.csv", header=T)
runecopath=function (ecopathinput) 
{
# assign input to variables
B=ecopathinput$Biomass
PB=ecopathinput$PB
QB=ecopathinput$QB
EE=ecopathinput$EE
BA=ecopathinput$BA
DC=data.frame(cbind(ecopathinput[11:25]))
Y=ecopathinput$Yield
Grouptype=ecopathinput$Type

n=length(B)
# Parindex is a vector of 1 and 0, 0 indicates that biomass is specified, 1
# indicates that EE is specified
Parindex=rep (0,n)

# find groups where  biomass is an input
Bindex=which(B>0)

# find groups where EE is an input
EEindex=which(EE>0)

# setup some matrices for calculations

QBmat=matrix(t(QB),nrow=n,ncol=1)
Parindex(EEindex,1)=seq(1,length(EEindex),1)                       

# Calculate known consumption rates on each prey group
if (length(Bindex)==0) # do this if no biomass levels are specified
   	{
     nr=length(Bindex)
   	KnownCons=rep(0,n)
   	
   for (i in 1:nr) # otherwise, cycle through all groups for which biomass is an input
      {
      KnownCons=KnownCons+B[Bindex[i]]*QB[Bindex[i]]*DC[,Bindex[i]]
      Parindex[Bindex[i,1]]=2
   }
}

# Calculate total losses for each group
G=Y+KnownCons+BA

# Solve for linear coefficients
a=matrix(0,nrow=n, ncol=n)
for (i in 1:length(B)) {
   for (j in 1:length(B)) {
      if (i==j) {
         if (Parindex[i]==1) {
            a[i,j]=EE[i]*PB[i]-QB[i]*DC[i,i]
        } else
            a[i,j]=B[i]*PB[i]
         
        }else if (Parindex[j]==1)
            a[i,j]=-DC[i,j]*QB[j]
   }
}

# make corrections for primary producer group
Groupindex=which(Grouptype==1)
for (i in 1:length(Groupindex)) {
    index=Groupindex[i]
    if (Parindex[index]==1) {
       a[index,index]=EE[index]*PB[index]
    } else
        a[index,index]=B[index]*PB[index]
}


# Solve equation
X=solve(a)*G

#Make Output
Bout=rep(0,n);EEout=rep(0,n)
for (i in 1:length(B)) {
   if (Parindex[i]==1) {
      Bout[i]=X[i]
      EEout[i,1]=EE[i]
    } else
      Bout[i]=B[i]
      EEout[i,1]=X[i]
}


# Calculate trophic levels
source("makeTL.R")
TL=makeTL(DC,Grouptype)


#--------------------------------------------------------------
# Calculate Consumption and Mortalities
for (j in 1:n) {
   # Calculate consumption (CC) on this group
   CC[j,1]=sum(t(DC[j,])*QB*Bout)
}
# Calculate Predation Mortalities:
Mp=CC/Bout
# Calcluate Fishing Mortalities
FF=Y/Bout
# Calcluate Other Mortality
Mo=PB-Mp-FF-BA/Bout

# Calculate Predation Matrix
for (i in 1:n) {
   Consumption=QB[i]*Bout[i]
   Mpmat[,i]=Consumption*DC[,i]/Bout
}

# Calculate Gross Conversion Efficiency
GCE=PB/QB
res=list(Bout=Bout, PB=PB, EEout=EEout, QB=QB, DC=DC, BA=BA, TL=TL, Mpmat=MPmat, Mo=Mo, GCE=GCE)
return (res)
}  #end function

                