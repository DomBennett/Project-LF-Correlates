loopThroughTests <- function(mdl_data, vrbls, mtrc, grp='All') {
  res <- data.frame(grp=NA, n=NA, x=mtrc, y=NA, frmla=NA,
                    int=NA, slp=NA, AIC=NA, p=NA)
  for(nm_vrbl in vrbls) {
    cat('.... [', nm_vrbl, ']\n')
    x <- mdl_data[[nm_vrbl]]
    y <- mdl_data[[mtrc]]
    nm <- mdl_data[['scinm']]
    genus <- mdl_data[['genus']]
    family <- mdl_data[['family']]
    order <- mdl_data[['order']]
    data <- data.frame(y, x, nm, genus, family, order)
    data[['x']][data[['x']] == -Inf] <- NA
    data[['x']][data[['x']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 50) {
      next
    }
    # select NULL model
    ms <- vector("list", length=8)
    ms[[1]] <- lm(y~1, data=data)
    ms[[2]] <- lmer(y~1+(1|genus), data=data, REML=FALSE)
    ms[[3]] <- lmer(y~1+(1|family), data=data, REML=FALSE)
    ms[[4]] <- lmer(y~1+(1|order), data=data, REML=FALSE)
    ms[[5]] <- lmer(y~1+(1|family/genus), data=data, REML=FALSE)
    ms[[6]] <- lmer(y~1+(1|order/family), data=data, REML=FALSE)
    ms[[7]] <- lmer(y~1+(1|order/genus), data=data, REML=FALSE)
    ms[[8]] <- lmer(y~1+(1|order/family/genus), data=data, REML=FALSE)
    nulli <- which.min(sapply(ms, AIC))
    m0 <- ms[[which.min(sapply(ms, AIC))]]
    rm(ms)
    if(nulli == 1) {
      # no need for random effects
      m1 <- lm(y~x, data=data)
    } else if(nulli == 2) {
      # select fitted, shared slope
      m1 <- suppressWarnings(lmer(y~x+(1|genus), data=data, REML=FALSE))
      # select fitted, changing slope
      m2 <- suppressWarnings(lmer(y~x+(x|genus), data=data, REML=FALSE))
    } else if(nulli == 3) {
      m1 <- suppressWarnings(lmer(y~x+(1|family), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|family), data=data, REML=FALSE))
    } else if(nulli == 4) {
      m1 <- suppressWarnings(lmer(y~x+(1|order), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|order), data=data, REML=FALSE))
    } else if(nulli == 5) {
      m1 <- suppressWarnings(lmer(y~x+(1|family/genus), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|family/genus), data=data, REML=FALSE))
    } else if(nulli == 6) {
      m1 <- suppressWarnings(lmer(y~x+(1|order/family), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|order/family), data=data, REML=FALSE))
    } else if(nulli == 7) {
      m1 <- suppressWarnings(lmer(y~x+(1|order/genus), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|order/genus), data=data, REML=FALSE))
    } else {
      m1 <- suppressWarnings(lmer(y~x+(1|order/family/genus), data=data, REML=FALSE))
      m2 <- suppressWarnings(lmer(y~x+(x|order/family/genus), data=data, REML=FALSE))
    }
    if(exists('m2')) {
      # choose best model between m1 and m2
      anvres <- anova(m2, m1)
      if(anvres$`Pr(>Chisq)`[2] < 0.05) {
        m1 <- m2
      }
    }
    aics <- AIC(m0, m1)[,2]
    sm0 <- summary(m0)
    sm1 <- summary(m1)
    frmla <- as.character(sm1$call)[2]
    anvres <- anova(m0, m1)
    if(aics[1] < aics[2]) {
      p <- ' '
    } else if(anvres$`Pr(>Chisq)`[2] < 0.001) {
      p <- '***'
    } else if(anvres$`Pr(>Chisq)`[2] < 0.01) {
      p <- '**'
    } else if(anvres$`Pr(>Chisq)`[2] < 0.05) {
      p <- '*'
    } else if(anvres$`Pr(>Chisq)`[2] < 0.1) {
      p <- '.'
    } else {
      p <- ' '
    }
    int <- sm1$coefficients[1,1]
    slp <- sm1$coefficients[2,1]
    tmp <- data.frame(grp=grp, n=nrow(data), x=mtrc, y=nm_vrbl,
                      frmla=frmla, int=int, slp=slp,
                      AIC=aics[2], p=p)
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
