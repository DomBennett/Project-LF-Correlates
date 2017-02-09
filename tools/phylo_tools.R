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
  res <- data.frame(x=mtrc, y=NA, int=NA, slp=NA, AIC=NA, p=NA)
  for(nm_vrbl in vrbls) {
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
    if(p_val < 0.001) {
      p <- '***'
    } else if(p_val < 0.01) {
      p <- '**'
    } else if(p_val < 0.05) {
      p <- '*'
    } else if(p_val < 0.1) {
      p <- '.'
    } else {
      p <- ' '
    }
    int <- sm$tTable[1,1]
    slp <- sm$tTable[2,1]
    tmp <- data.frame(x=mtrc, y=nm_vrbl,
                      int=int, slp=slp,
                      AIC=AIC(m), p=p)
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
