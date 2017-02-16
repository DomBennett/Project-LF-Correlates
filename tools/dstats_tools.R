getCorrs <- function(mdl_data, vrbls, mtrc, grp='All') {
  res <- data.frame(grp=NA, n=NA, x=mtrc, y=NA, pearson=NA,
                    spearman=NA)
  for(nm_vrbl in vrbls) {
    x <- mdl_data[[nm_vrbl]]
    y <- mdl_data[[mtrc]]
    data <- data.frame(mdl_data$scinm, x, y)
    data[['x']][data[['x']] == -Inf] <- NA
    data[['x']][data[['x']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 20) {
      next
    }
    if(is.factor(data$x)) {
      next
    }
    if(var(data$x) == 0) {
      next
    }
    tmp <- data.frame(grp=grp, n=nrow(data), x=mtrc,
                      y=nm_vrbl, pearson=cor(data$x, data$y, method = 'pearson'),
                      spearman=cor(data$x, data$y, method = 'spearman'))
    res <- rbind(res, tmp)
  }
  res[-1, ]
}

getCatMeans <- function(mdl_data, vrbls, mtrc, grp='All') {
  res <- data.frame(grp=NA, n=NA, ncats=NA, x=mtrc, y=NA,
                    hghst=NA, lwst=NA, dff=NA)
  for(nm_vrbl in vrbls) {
    x <- mdl_data[[nm_vrbl]]
    y <- mdl_data[[mtrc]]
    data <- data.frame(x, y)
    data[['x']][data[['x']] == -Inf] <- NA
    data[['x']][data[['x']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 20) {
      next
    }
    if(!is.factor(data$x)) {
      next
    }
    means <- tapply(data$y, data$x, mean)
    hghst <- names(means)[which.max(means)]
    lwst <- names(means)[which.min(means)]
    tmp <- data.frame(grp=grp, n=nrow(data), ncats=length(means), x=mtrc,
                      y=nm_vrbl, hghst=hghst, lwst=lwst, dff=means[hghst]-means[lwst])
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
