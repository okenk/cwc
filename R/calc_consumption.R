# # Program to load in Ecopath files and create vectors and matrices as needed for calculations.  
##############################

calc_consumption <- function(rpath.obj, rm.weak.links = FALSE) {
  ## SET UP VECTORS###############
  n.groups <- with(rpath.obj, NUM_GROUPS - NUM_GEARS)
  Group.Type.all <- c('Consumer', 'Producer', 'nonliving', 'gear')[rpath.obj$type+1]
  Group.Type <- Group.Type.all[Group.Type.all != 'gear']
  consumer.groups=which(Group.Type=="Consumer")
  n.consumer.groups=length(consumer.groups)
  
  B=as.matrix(rpath.obj$Biomass[Group.Type.all != 'gear'])
  QB=as.matrix(rpath.obj$QB[Group.Type.all != 'gear'])
  p.init=as.matrix(rpath.obj$DC[1:n.groups, ])
  if(dim(p.init)[2] < n.groups) {
    p <- cbind(p.init, matrix(0, nrow = n.groups, ncol = n.groups - dim(p.init)[2]))
    Import.feeding=as.matrix(c(rpath.obj$DC[nrow(rpath.obj$DC),], 
                               rep(0, n.groups - dim(p.init)[2]))) # last row of DC matrix in rpath is imports. I think. 
    warning('Appending column of zeros to diet matrix. Assume missing column is for producer or detritus "diet".')
  } else {
    p <- p.init
    Import.feeding=as.matrix(rpath.obj$DC[nrow(rpath.obj$DC),]) # last row of DC matrix in rpath is imports. I think. 
  }
  #######################################
  #  make sure that diet matrices add up to 1 perfectly
  sum.of.p<-colSums(p)+Import.feeding
  # for groups that have no diet, just put a 1 in for sum of p
  is.zero<-which(sum.of.p==0)
  if (length(is.zero)>0) sum.of.p[is.zero]=rep(1,length(is.zero))
  sump<-matrix(sum.of.p,nrow=n.groups,ncol=n.groups,byrow=TRUE)
  new.p<-p/sump
  p<-new.p
  import.notzero<-which(colSums(p)>0)
  Import.feeding[import.notzero]<-Import.feeding[import.notzero]/sum.of.p[import.notzero]
  
  #######################################
  
  ## CALCULATE CONSUMPTION MATRIX
  B.QB=c(B*QB) # total consumption
  na.index=which(is.na(B.QB)==TRUE)
  B.QB[na.index]=rep(0,length(na.index))
  d.B.QB=diag(B.QB,nrow=n.groups,ncol=n.groups)
  consump=p%*%d.B.QB
 
  ## OPTIONAL SWITCH TO REMOVE SMALL LINKS
  
  if (rm.weak.links){
    prey.link.thresh<-0.025
    pred.link.thresh<-0.025
    p.small<-which(p<prey.link.thresh&p>0)
    pred.small<-which(MpM<pred.link.thresh&MpM>0)
    all.2.zero<-intersect(p.small,pred.small)
    # Replace diet matrix with 0
    if (length(all.2.zero)>0) {
      new.p<-replace(p,all.2.zero,0)
      sigma.p<-colSums(new.p)+Import.feeding
      # Standardize P for living groups
      for (grp in 1:n.consumer.groups){
        consumer.id<-consumer.groups[grp]
        summed.p<-sigma.p[consumer.id]
        new.p[,consumer.id]<-new.p[,consumer.id]/summed.p
        Import.feeding[consumer.id]<-Import.feeding[consumer.id]/summed.p
      }
      p<-new.p
      # Update production / Mo and other parmaeters
      consump=p%*%d.B.QB
     }
  }
  return(list(consump=consump, Import.feeding=Import.feeding, p=p))
}