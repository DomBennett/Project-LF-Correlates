# record models
cache_dir <- 'caches'
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}
cache_dir <- file.path('caches', 'phylo')
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}
if(!file.exists(file.path(cache_dir, 'skip.RData'))) {
  skip_mdl <- NULL
  save(skip_mdl, file=file.path(cache_dir, 'skip.RData'))
} else {
  load(file=file.path(cache_dir, 'skip.RData'))
}
skipMdl <- function(fl) {
  skip_mdl <<- c(skip_mdl, fl)
  save(skip_mdl, file=file.path(cache_dir, 'skip.RData'))
}

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
  res <- data.frame(n=NA, x=mtrc, y=NA, int=NA, slp=NA,
                    AIC=NA, null=NA, p=NA)
  for(nm_vrbl in vrbls) {
    fl <- file.path(cache_dir, paste0(mtrc, '_', nm_vrbl, '.RData'))
    if(fl %in% skip_mdl) {
      next
    }
    if(file.exists(fl)) {
      load(fl)
      res <- rbind(res, tmp)
      next
    }
    cat('....[', nm_vrbl, ']\n')
    vrbl <- epi[[nm_vrbl]]
    pepi <- epi[[mtrc]]
    data <- data.frame(vrbl, pepi)
    row.names(data) <- epi[['scinm']]
    data[['vrbl']][data[['vrbl']] == -Inf] <- NA
    data[['vrbl']][data[['vrbl']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 20) {
      skipMdl(fl)
      next
    }
    to_drp <- mammal_tree$tip.label[!mammal_tree$tip.label %in% rownames(data)]
    tree <- drop.tip(mammal_tree, to_drp)
    m0 <- gls(pepi~1, data=data, method="ML",
              correlation=corPagel(value=1, phy=tree, fixed=FALSE))
    m1 <- gls(pepi~vrbl, data=data, method="ML",
              correlation=corPagel(value=1, phy=tree, fixed=FALSE))
    sm <- summary(m1)
    anvres <- anova(m0, m1)
    p_val <- anvres[['p-value']][2]
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
    tmp <- data.frame(n=nrow(data),x=mtrc, y=nm_vrbl,
                      int=int, slp=slp,
                      AIC=AIC(m1), null=AIC(m0),
                      p=p)
    save(tmp, file=fl)
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
