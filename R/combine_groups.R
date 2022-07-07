remove_rows_rename <- function(name.indexes, to.modify, new.names) {
  to.modify <- to.modify[-name.indexes[-1]]
  names(to.modify) <- new.names
  return(to.modify)
}

combine_groups <- function(rpath.mod, grp.names, combined.name) {
  
  combined.mod <- rpath.mod
  name.indexes <- sort(which(combined.mod$Group %in% grp.names))
  
  # Fix counts
  combined.mod$NUM_GROUPS <- rpath.mod$NUM_GROUPS - length(grp.names) + 1
  combined.mod$NUM_LIVING <- rpath.mod$NUM_LIVING - length(grp.names) + 1
  
  # Combine group names
  combined.mod$Group[name.indexes[1]] <- combined.name
  names(combined.mod$Group)[name.indexes[1]] <- combined.name
  combined.mod$Group <- combined.mod$Group[-name.indexes[-1]]
  
  # type
  combined.mod$type <- remove_rows_rename(name.indexes, rpath.mod$type, 
                                          combined.mod$Group)
  
  # make TL, BA, Unassim, GE = NA for combined group 
  # (do not recalculate, are not used in the generalized model)
  for(model.variable in c('TL', 'BA', 'Unassim', 'GE')) {
    combined.mod[[model.variable]] <- remove_rows_rename(name.indexes, 
                                                         rpath.mod[[model.variable]],
                                                         combined.mod$Group)
    combined.mod[[model.variable]][name.indexes[1]] <- NA
  }
  
  # Add biomassesu
  combined.mod$Biomass <- remove_rows_rename(name.indexes, 
                                             rpath.mod$Biomass,
                                             combined.mod$Group)
  combined.mod$Biomass[name.indexes[1]] <- sum(rpath.mod$Biomass[name.indexes])
  
  # PB is biomass-weighted average
  combined.mod$PB <- remove_rows_rename(name.indexes, rpath.mod$PB,
                                        combined.mod$Group)
  combined.mod$PB[name.indexes[1]] <- sum(rpath.mod$Biomass[name.indexes] *
                                            rpath.mod$PB[name.indexes]) / 
    sum(rpath.mod$Biomass[name.indexes])
  
  # QB is biomass-weighted average
  combined.mod$QB <- remove_rows_rename(name.indexes, rpath.mod$QB,
                                        combined.mod$Group)
  combined.mod$QB[name.indexes[1]] <- sum(rpath.mod$Biomass[name.indexes] *
                                            rpath.mod$QB[name.indexes]) / 
    sum(rpath.mod$Biomass[name.indexes])
  
  # EE is biomass-weighted average
  combined.mod$EE <- remove_rows_rename(name.indexes, rpath.mod$EE,
                                        combined.mod$Group)
  combined.mod$EE[name.indexes[1]] <- sum(rpath.mod$Biomass[name.indexes] *
                                            rpath.mod$EE[name.indexes]) / 
    sum(rpath.mod$Biomass[name.indexes])
  
  # Sum DC for consumption of the group. 
  combined.mod$DC <- rpath.mod$DC[-name.indexes[-1], -name.indexes[-1]]
  rownames(combined.mod$DC)[1:combined.mod$NUM_LIVING] <- 
    colnames(combined.mod$DC)[1:combined.mod$NUM_LIVING] <- 
    combined.mod$Group[1:combined.mod$NUM_LIVING]
  combined.mod$DC[name.indexes[1],] <- 
    colSums(rpath.mod$DC[name.indexes, -name.indexes[-1]])
  
  # DC by the group is consumption-weighted average
  temp <- t(rpath.mod$DC[,name.indexes]) %>%
    multiply_by(rpath.mod$QB[name.indexes]) %>%
    multiply_by(rpath.mod$Biomass[name.indexes]) %>% 
    colSums() / 
    sum(rpath.mod$QB[name.indexes] * rpath.mod$Biomass[name.indexes])
  # sum up groups to be combined and remove excess entries
  temp[name.indexes[1]] <- sum(temp[name.indexes])
  temp <- temp[-name.indexes[-1]]
  combined.mod$DC[,name.indexes[1]] <- temp
  
  # detfate: not used in generalized model, NA this
  combined.mod$DetFate <- rpath.mod$DetFate[-name.indexes[-1],, 
                                            drop = FALSE]
  combined.mod$DetFate[name.indexes[1],] <- NA
  rownames(combined.mod$DetFate) <- combined.mod$Group
  
  # sum fishery landings
  combined.mod$Landings <- rpath.mod$Landings[-name.indexes[-1],, 
                                              drop = FALSE]
  combined.mod$Landings[name.indexes[1],] <- 
    apply(rpath.mod$Landings[name.indexes,, drop = FALSE], 2, sum)
  rownames(combined.mod$Landings) <- combined.mod$Group
  
  # sum discards
  combined.mod$Discards <- rpath.mod$Discards[-name.indexes[-1],, 
                                              drop = FALSE]
  combined.mod$Discards[name.indexes[1],] <- 
    apply(rpath.mod$Discards[name.indexes,, drop = FALSE], 2, sum)
  rownames(combined.mod$Discards) <- combined.mod$Group
  
  return(combined.mod)
}
