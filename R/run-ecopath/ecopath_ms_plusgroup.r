
 #function [Bout QBout wa Nout alpha a wa_plusgroup ca_plusgroup]=ecopath_ms_plusgroup(Bleading,QBleading,Zs,ns,Startages,K,BA,wm,d)  #orig Matlab code
 ecopath_ms_plusgroup=function(Bleading,QBleading,Zs,Ns,StartAge_ms,K_ms,BA,wm,d)           #?? changed Startages = StartAge_ms
 {

ss=length(StartAge_ms)
K=K_ms
# Find age at 95% Winf
group_maxage=ceiling(-12*log(1-0.90^(1/3))/K_ms)                     ##?? changed K = K_ms
za=matrix(0,group_maxage+1,1)
# first make up Z's of ages;
for (i in 1:Ns)
{
    if (i<Ns)
    {
        maxage=StartAge_ms[i+1]-1
    } else {
        maxage=group_maxage
    }
    za[(StartAge_ms[i]+1):(maxage+1),1]=-Zs[i]/12*matrix(1,(maxage-StartAge_ms[i]+1),1)
}

la=matrix(0,group_maxage+1,1)
# Make la's, relative numbers at age
for (i in 1:length(za))
{
    if (i==1)
    {
        la[i]=1
    } else {
    	la[i]=exp(sum(za[1:i-1])-i*BA)  
	}
}
# Make correction for plus group;


tmax=100
Z=-za[length(za)]*12
la[length(la)]=-12*la[length(la)]*(exp(-(Z)*tmax)-1)/Z 
#     The loop above calculates the number of individuals surviving to the 
#     first age class of the plus group  la(end) is the inital abundance.  
#     The total abundance in the plus group is calculated by integrating 
#     N(t)dt=Noexp(-Zt)dt over t.  tmax is arbitrarily set at 100 years. 
#     
#Make Na's (the proportion of individuals in each size category
Na=la/sum(la)       
n=length(Na)
#make weight at age (wa's)
tlist=seq(0,maxage,1)
wa=(matrix(1,length(tlist),1)-exp(-K/12*(tlist)))^3
# calculate mean body size for plus group;
# Mean body size is the weighted average body size, integrated over all of
# the ages in the plus group.  In other words, its the integral of N(t)x
# w(t) dt divided by the integral N(t)dt.  If t equals 0 at the age at
# entry into the plus group (tstart), then size at age equals
# (1-exp(-k(t+tstart))^3.  I calculated the integral using the symbolic
# math solver (nasty integral!)
tstart=group_maxage/12
w=length(wa)

wa[w]=(9*K^2*exp(K*(tstart-2*tmax))*Z-15*K*exp(K*(2*tstart-tmax))*Z^2+12*K*exp(K*(tstart-2*tmax))*Z^2-3*K*exp(-3*K*tmax)*Z^2-3*exp(K*(2*tstart-tmax))*Z^3+3*exp(K*(tstart-2*tmax))*Z^3+exp(Z*tmax)*Z^3+Z^3*exp(3*K*tstart)-18*K^2*exp(K*(2*tstart-tmax))*Z+6*Z^2*K*exp(3*K*tstart)+11*K^2*Z*exp(3*K*tstart)+6*K^3*exp(3*K*tstart)+2*K^2*Z*exp(Z*tmax)+3*K*exp(Z*tmax)*Z^2-exp(-3*K*tmax)*Z^3-11*K^2*exp(Z*tmax+3*K*tstart)*Z+18*K^2*exp(Z*tmax+2*K*tstart)*Z-9*K^2*exp(Z*tmax+K*tstart)*Z-6*K*exp(Z*tmax+3*K*tstart)*Z^2-12*K*exp(Z*tmax+K*tstart)*Z^2+15*K*exp(Z*tmax+2*K*tstart)*Z^2-2*K^2*exp(-3*K*tmax)*Z-3*exp(Z*tmax+K*tstart)*Z^3-exp(Z*tmax+3*K*tstart)*Z^3-6*K^3*exp(Z*tmax+3*K*tstart)+3*exp(Z*tmax+2*K*tstart)*Z^3)*exp(-Z*tmax-3*K*tstart)/(Z^3+6*Z^2*K+11*Z*K^2+6*K^3)/(exp(-Z*tmax)-1)

# Calculate_biomass at age--------------------------------------
ba=Na*wa
basum=sum(ba)


# Now calculate relative biomass in each stanza
Bs=matrix(0,ss,1)
for (i in 1:Ns)
{
    if (i<Ns)
    {
        Bs[i,1]=sum(ba[(StartAge_ms[i]+1):StartAge_ms[i+1]])/basum
    } else
        Bs[i,1]=sum(ba[(StartAge_ms[i]+1):length(ba)])/basum
    
}

# Total biomass of all groups equals bleading / bs(leading)
Btotal=Bleading/Bs[ss]
Bout=Btotal*Bs

# Calculate consumption for each age class

C_leading=Bleading*QBleading
ca=Na*wa^(d)
cc=length(ca)
# Make correction for plus group (calculate average consumption rate) by
# integrating w(t)d x N(t) dt divided by N(t) dt.
# An analytic solution is only possible if d = 2/3;  Otherwise, it has to
# be numerically integrated
if (d==2/3)
{
    ca[cc]=Na[n]*(-2*exp(Z*tmax+2*K*tstart)*K^2+2*K^2*exp(2*K*tstart)-4*K*exp(K*(-tmax+tstart))*Z+3*K*Z*exp(2*K*tstart)-K*Z*exp(Z*tmax)+4*K*exp(Z*tmax+K*tstart)*Z-3*K*exp(Z*tmax+2*K*tstart)*Z+K*exp(-2*K*tmax)*Z-exp(Z*tmax)*Z^2+2*exp(Z*tmax+K*tstart)*Z^2+Z^2*exp(2*K*tstart)+exp(-2*K*tmax)*Z^2-2*exp(K*(-tmax+tstart))*Z^2-exp(Z*tmax+2*K*tstart)*Z^2)*exp(-Z*tmax-2*K*tstart)/(Z^2+3*Z*K+2*K^2)/(exp(-Z*tmax)-1)
 } else {
    ca[cc]=solve_consumption_int(Z,K,d,tstart,tmax)
    ca[cc]=ca[cc]*Na[n]
}

casum=sum(ca)
cs=matrix(0,Ns,1)
for (i in 1:Ns)
{
    if (i<Ns)
    {
        cs[i,1]=sum(ca[(StartAge_ms[i]+1):StartAge_ms[i+1]])/casum
    }else
        cs[i,1]=sum(ca[(StartAge_ms[i]+1):length(ca)])/casum
    
}

ccs=length(cs)
# Calculate QB's from this
C_total=C_leading/cs[ccs]

Cout=C_total*cs
QBout=Cout/Bout

# Calculate Total Number of Individuals
firstrow=StartAge_ms[length(StartAge_ms)]+1
lastrow=length(ba)

Ntotal=Bleading/sum(ba[firstrow:lastrow])
Nout=Ntotal*Na


###################
# Calculate SPR
matureindex=which(wa>=wm)

SPR=sum(la[matureindex]*(wa[matureindex]-wm*matrix(1,length(matureindex),1)))
# Alpha is the mass-specific fecundity
alpha = 1/SPR

# Calculate net assimilation efficiency
a = (1/(1-d))* K *Ntotal*sum(ca)/C_total


wa_plusgroup=wa[w]
ca_plusgroup=ca[cc]

res=list(Bout=Bout, QBout=QBout, wa=wa, Nout=Nout,wa_ms=wa, alpha=alpha, a=a,  wa_plusgroup=wa_plusgroup, ca_plusgroup=ca_plusgroup)
return(res)
}      #end function


