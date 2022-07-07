# This is general script that (1) allows for Ecosim initialization
#  parameters and (2) runs Ecosim in user-defined time steps (default is
#  monthly).  This script has the capacitity to work with Multi-stanza
#  routines, though this element has not been directly tested yet with EwE.
#
#----------------------------- Initialize Ecosim
ecopathinput=read.csv("CNPTestmodel.csv", header=T)
nyears=50
delta_t=1/12
tlist=seq(0,nyears,(1/12))                                        
Flist=( t(tlist) rep(1,tlist,1)] #  vector of multipliers of ecopath fishing mortalities
vuln=data.frame(cbind(ecopathinput[26:40])) # these are the specified vulnerabilities

# Initialize functional response paramters
source("makev.r")
makev(Mpmat,B,vuln,ecopathinput$Type)
vmat=res$vmat; amat=res$amat      #rename function outputs
Bprior=B
Boutput[,1]=Bprior
Youtput[,1]=Y

#------------------------------------------- Run ecosim
for ( i in 2:length(tlist)) {
    Fmult=Flist[i-1,2]
    Ftemp=FF*Fmult
    #X=ecosim_runge_kutta(deltat,[Bprior; zeros(ngroups,1); zeros(ngroups,1)],amat,vmat,Mo,F,Type,PB,GCE,Fmult);
    Bnew=ecosim_runge_kutta(delta_t,Bprior,amat,vmat,Mo,F,ecopathinput$Type,PB,GCE)
    Yields=Ftemp*Bprior*(rep(1,ngroups)-Bnew/Bprior)/(-log(Bnew/Bprior))

    # Now go through each multi stanza group;
    if (n_ms_groups>0) {
        for (j in 1:n_ms_groups) {
            group=j
            group_index=which(Group==group)
            [Nanext wanext]=multa_stanza_calculate(group_index,Bprior,Bnew,group,ms_info,Consumption, Predation,Mo_ecopath,F_ecopath*Fmult)  ##missing this function file?
            tempstring=sprintf('ms_info.Na%d=%s;',group,'Nanext')
            eval(tempstring)
            tempstring=sprintf('ms_info.wa%d=%s;',group,'wanext')
            eval(tempstring)
        }
    }
    Boutput(,i)=Bnew
    Youtput(,i)=Yields
    Bprior=Bnew
}



