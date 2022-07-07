 ecopathinput=read.csv("CNPTestmodel.csv", header=T)
# Simple ecopath model that demonstrates the format of data input. This
# script file creates a structure array called ecopathinput with fields:
# Biomass (B), EE, PB, QB, BA (biomass accumulation), Yield, Type (consumer,
# producer, detritus), DC (diet composition), vuln (ecosim vulnerabilities).  Additional fields are used for multistanza routines
# Input biomass (-1 is a flag for matlab to solve for biomass
# Input Ecotrophic Efficiencies : -1 is a flat for matlab to solve for EE
# input multistanza groups (label linked groups with same number). 0 means no multistanza group
# "Startage" Input start age for multistanza groups
# "K" input von-B K for multistanza groups
# "DC" diet composition
# "vuln" vulnerability settings


 ecopathinput=na.omit(ecopathinput)  #remove missing data (NA or empty space so that 'max' doesn't return NA)
# Routine for Multi-Stanza groups
n_ms_groups=max(ecopathinput$Group)  # number of multi stanza groups 

if (n_ms_groups>0)        
{ 
    # cycle through each multi stanza group and get B, PB and QB and create
    # structure array of multi stanza parameters
    for (i in 1:n_ms_groups)
    {
        wm=0.25 # relative weight at maturity
        d=2/3 # set the allometry of consumption to 2/3
        group_index=which(ecopathinput$Group==i)     
        B_ls=ecopathinput$Biomass[max(group_index)]        
        PB_ms=ecopathinput$PB[group_index]
        QB_ls=ecopathinput$QB[max(group_index)]
        BA_ms=ecopathinput$BA[max(group_index)]
        StartAge_ms=ecopathinput$StartAge[group_index]
        K_ms=ecopathinput$K[max(group_index)]
        Ns=length(group_index)
        source("ecopath_ms_plusgroup.R")
        ecopath_ms_plusgroup(B_ls,QB_ls,PB_ms,Ns,StartAge_ms,K_ms,BA_ms,wm,d)
        B_ms=res$B_ms; QB_ms=res$QB_ms; wa_ms=res$wa_ms; N_ms=res$N_ms; alpha=res$alpha; a=res$a; wa_plusgroup=res$wa_plusgroup; ca_plusgroup=res$ca_plusgroup  #rename function outputs from returned list 'res"
        ecopathinput$Biomass[group_index]=B_ms
        ecopathinput$QB[group_index]=QB_ms
        
        #Save the vector of weights and numbers to structure arrays
            tempstring=sprintf('ms_info.wa%d is %s;',i,'wa_ms')
            eval(tempstring)
            tempstring=sprintf('ms_info.Na%d is %s;',i,'N_ms')
            eval(tempstring)
            tempstring=sprintf('ms_info.StartAge%d is %s;',i,'StartAge_ms')
            eval(tempstring)
            tempstring=sprintf('ms_info.groupindex%d is %s;',i,'group_index')
            eval(tempstring)
            a_output[i,1]=a
            alpha_output[i,1]=alpha
            K_mat[i]=K_ms
        # --------------------------------------------------
          }
          
ms_info.a=a_output
ms_info.alpha=alpha_output
ms_info.d=dmat
ms_info.wm=wmmat
ms_info.K=K_mat
}

# Run ecopath routine, and extract TL, matrix of predation mortality rates
# and (Mpmat) and vector of other mortality (Mo)
source("runecopath.R")
runecopath(ecopathinput)       # in other file - note in output get Bout not B and Eout not E
B=res$Bout
E=res$Eout ##added these to get outputs in correct names



