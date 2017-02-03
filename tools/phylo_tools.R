getMdlData <- function(vrbls, mtrc='pepi') {
  mtrc <- epi[[mtrc]]
  data <- data.frame(mtrc)
  for(vrbl in vrbls) {
    data[[vrbl]] <- epi[[vrbl]]
    data[[vrbl]][data[[vrbl]] == -Inf] <- NA
    data[[vrbl]][data[[vrbl]] == Inf] <- NA
  }
  row.names(data) <- epi[['scinm']]
  data <- na.omit(data)
  data
}

loopThroughTests <- function(mtrc) {
  mdl_res <- list()
  for(nm_vrbl in vrbls) {
    cat('.... [', nm_vrbl, ']\n')
    vrbl <- epi[[nm_vrbl]]
    pepi <- epi[[mtrc]]
    data <- data.frame(vrbl, pepi)
    row.names(data) <- epi[['scinm']]
    data[['vrbl']][data[['vrbl']] == -Inf] <- NA
    data[['vrbl']][data[['vrbl']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 20) {
      next
    }
    to_drp <- mammal_tree$tip.label[!mammal_tree$tip.label %in% rownames(data)]
    tree <- drop.tip(mammal_tree, to_drp)
    m <- gls(pepi~vrbl, data=data, method="ML",
             correlation=corPagel(value=1, phy=tree, fixed=FALSE))
    sm <- summary(m)
    p_val <- sm$tTable[2,4]
    slp <- sm$tTable[2,1]
    if(p_val < 0.05) {
      cat('.... [', vrbl, '] is significant, est: [',
          slp, ']\n', sep='')
      mdl_res[[nm_vrbl]] <- sm
    }
  }
  mdl_res
}
