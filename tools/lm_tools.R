loopThroughTests <- function(mdl_data, vrbls, mtrc, grp='All') {
  res <- data.frame(grp=NA, n=NA, x=mtrc, y=NA, pr=NA,
                    int=NA, slp=NA, AIC=NA, p=NA)
  for(nm_vrbl in vrbls) {
    cat('.... [', nm_vrbl, ']\n')
    x <- mdl_data[[nm_vrbl]]
    y <- mdl_data[[mtrc]]
    data <- data.frame(x, y)
    data[['x']][data[['x']] == -Inf] <- NA
    data[['x']][data[['x']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 200) {
      next
    }
    m0 <- lm(y~1, data=data)
    m1 <- lm(y~x, data=data)
    aics <- AIC(m0, m1)[,2]
    sm0 <- summary(m0)
    sm1 <- summary(m1)
    frmla <- as.character(sm1$call)[2]
    anvres <- anova(m0, m1)
    if('Pr(>Chisq)' %in% names(anvres)) {
      p_vl <- anvres[['Pr(>Chisq)']][2]
    } else {
      p_vl <- anvres[['Pr(>F)']][2]
    }
    if(aics[1] < aics[2]) {
      p <- ' '
    } else if(p_vl < 0.001) {
      p <- '***'
    } else if(p_vl < 0.01) {
      p <- '**'
    } else if(p_vl < 0.05) {
      p <- '*'
    } else if(p_vl < 0.1) {
      p <- '.'
    } else {
      p <- ' '
    }
    int <- sm1$coefficients[1,1]
    slp <- sm1$coefficients[2,1]
    pr <- cor(data[['x']], data[['y']])
    tmp <- data.frame(grp=grp, n=nrow(data), x=mtrc, y=nm_vrbl,
                      pr=pr, int=int, slp=slp,
                      AIC=aics[2], p=p)
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
