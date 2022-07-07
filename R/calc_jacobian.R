calc_jacobian <- function(rpath.obj, theta.use, eps.use, gamma.use, rm.weak.links = FALSE) {
  # model to run yield trade-offs for a generalized production function
  # Must specify the theta (prey dependence), epsilon (predator dependence)
  # in functional repsonse as well as gamma - density dependence in PB ratio
  
  ###################################################
  # set up calculations for jacobian
  ##################################################
  if(min(which(rpath.obj$type==3)) < max(which(rpath.obj$type<3))) {
    error('Please rearrange gear groups in rpath so that fishing gears (type = 3) are last')
  }
  
  Group.Type.all <- c('Consumer', 'Producer', 'nonliving', 'gear')[rpath.obj$type+1]
  Group.Type <- Group.Type.all[Group.Type.all != 'gear']
  
  living.groups=which(Group.Type!="nonliving")
  n.living.groups=length(living.groups)
  producer.groups=which(Group.Type=="Producer")
  detritus.groups=which(Group.Type=="nonliving")
  n.detritus.groups=length(detritus.groups)
  ## CALCULATE F, CONSUMPTION MATRIX AND ALPHAS
  GCE=with(rpath.obj, PB/QB)[Group.Type.all != 'gear']
  GCE[producer.groups]=rep(0,length(producer.groups))
  GCE.living<-GCE[living.groups]
  ## Solve for M0, assuming no BA
  
  consump.ls <- calc_consumption(rpath.obj, rm.weak.links = FALSE)
  consump <- consump.ls$consump
  Import.feeding <- consump.ls$Import.feeding
  p <- consump.ls$p
  
  fishing <- with(rpath.obj, (Landings+Discards)/Biomass)[Group.Type.all != 'gear']
  PB <- rpath.obj$PB[Group.Type.all != 'gear']
  Mpred=rowSums(consump)/rpath.obj$Biomass[Group.Type.all != 'gear']
  Mo=PB-fishing-Mpred
  if(sum(Mo < 0) > 0) 
    warning(paste('Adjusted PB for groups', which(Mo < 0), 'due to non-zero biomass accumulation'))
  
  # Adjust PB as needed if Mo is negative
  PB<-PB-pmin(0,Mo) # essentially adding the production needed to make Mo = 0
  Mo<-PB-fishing-Mpred # get new "Mo" with adjusted PB, should be 0 if PB is adjusted
  
  B.living<-rpath.obj$Biomass[living.groups]
  Mo.living<-Mo[living.groups]
  QB.living<-rpath.obj$QB[living.groups]
  C.living<-(consump[living.groups,living.groups])
  # set up matrices of biomass in columns (x.i) and in rows (x.j)
  x.i<-matrix(B.living,nrow=n.living.groups,ncol=n.living.groups,byrow=FALSE)
  x.j<-matrix(B.living,nrow=n.living.groups,ncol=n.living.groups,byrow=TRUE)
  # set up theta,, epsilon and gamma coefficients
  theta<-matrix(theta.use,nrow=n.living.groups,ncol=n.living.groups)
  eps<-matrix(eps.use,nrow=n.living.groups,ncol=n.living.groups)
  gamma<-matrix(gamma.use,nrow=n.living.groups,ncol=1)
  
  # fix consumption from detritus and imports
  detritus.eps<-matrix(1, nrow=n.living.groups,ncol=1)
  import.eps<-matrix(1, nrow=n.living.groups,ncol=1)
  
  # apply exponents to relevant biomass levels
  x.j.eps<-x.j^eps
  x.i.theta<-x.i^theta
  theta.minus.1<-theta-matrix(1,nrow=n.living.groups,ncol=n.living.groups)
  eps.minus.1<-eps-matrix(1,nrow=n.living.groups,ncol=n.living.groups)
  gamma.minus.1<-gamma-matrix(1,nrow=n.living.groups,ncol=1)
  eps.minus.2<-eps-matrix(2,nrow=n.living.groups,ncol=n.living.groups)
  theta.minus.2<-theta-matrix(2,nrow=n.living.groups,ncol=n.living.groups)
  x.i.theta.minus.1<-x.i^theta.minus.1
  x.j.eps.minus.1<-x.j^eps.minus.1
  x.j.eps.minus.2<-x.j^eps.minus.2
  x.i.theta.minus.2<-x.i^theta.minus.2
  PP.eq<-matrix(0,nrow=n.living.groups,ncol=1)
  PP.eq[producer.groups]<-PB[producer.groups]
  P.eq<-PP.eq/diag(x.j.eps.minus.1)
  # This ensures that @ equilibrium, other mortality in generalized model
  # equals Mo from Ecopath calculations
  Moeq.living<-Mo[living.groups]/(B.living^gamma)
  
  # calculate alphas
  alpha<-C.living/(x.j.eps*x.i.theta)
  
  # calculate consumption on detritus and import consumption
  if (n.detritus.groups>0){
    if (n.detritus.groups>1){ 
      p.detritus=colSums(p[detritus.groups,living.groups])
    } else {
      p.detritus=p[detritus.groups,living.groups]
    }
    alpha.detritus=consump[detritus.groups, living.groups]/(B.living^detritus.eps)
    alpha.detritus[producer.groups]=0
  }
  
  Import.consumption=QB.living*Import.feeding[living.groups]*B.living
  Import.consumption[producer.groups]=0
  alpha.import<-Import.consumption/B.living^import.eps
  
  # Calculate jacobian
  Jr=matrix(0,nrow=n.living.groups,ncol=n.living.groups)
  for (i in 1:n.living.groups){
    index<-living.groups[i]
    if (Group.Type[index]=="Consumer"){
      for (j in 1:n.living.groups){
        if (j==i){
          # compute deriviative along the diagonal, slightly more complicated, but includes consumption on prey in model, cannibalism, on detritus and impor consumptions, minus predation and other mortality
          Jr[i,j]<-GCE.living[i]*sum(alpha[-i,i]*x.i.theta[-i,i]*eps.minus.1[-i,i]*x.j.eps.minus.2[-i,i])+
            (GCE.living[i]-1)*alpha[i,i]*(eps[i,i]+theta[i,i]-1)*B.living[i]^(eps[i,i]+theta[i,i]-2)+
            GCE.living[i]*alpha.detritus[i]*(detritus.eps[i]-1)*B.living[i]^(detritus.eps[i]-2)+
            GCE.living[i]*alpha.import[i]*(import.eps[i]-1)*B.living[i]^(import.eps[i]-2)-
            sum(alpha[i,-i]*x.i.theta.minus.2[i,-i]*theta.minus.1[i,-i]*x.j.eps[i,-i])-
            gamma[i]*Moeq.living[i]*B.living[i]^(gamma.minus.1[i])
        } else {
          # use these terms in the off diagonal
          Jr[i,j]<-GCE.living[i]*alpha[j,i]*theta[j,i]*x.i.theta.minus.1[j,i]*x.j.eps.minus.1[j,i]-
            alpha[i,j]*eps[i,j]*x.i.theta.minus.1[i,j]*x.j.eps.minus.1[i,j]
        }
      }
    } else {
      # this is for producers, I used eps to relate primary production to producer biomass
      for (j in 1:n.living.groups){
        if (j==i){
          Jr[i,j]<-P.eq[i]*x.j.eps.minus.2[i,j]*eps.minus.1[i,j]-
            sum(alpha[i,]*x.i.theta.minus.2[i,]*theta.minus.1[i,]*x.j.eps[i,])-gamma.minus.1[i]*Moeq.living[i]*B.living[i]^(gamma[i]-2)
        } else {
          Jr[i,j]<--alpha[i,j]*eps[i,j]*x.i.theta.minus.1[i,j]*x.j.eps.minus.1[i,j]
        }
      }
    }
  }
  return(Jr)
}