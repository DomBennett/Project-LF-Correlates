loopThroughTests <- function(mdl_data, vrbls, mtrc, grp='All') {
  res <- data.frame(grp=NA, n=NA, x=mtrc, y=NA, frmla=NA,
                    int=NA, slp=NA, NULL_AIC=NA, AIC=NA, p=NA)
  for(nm_vrbl in vrbls) {
    cat('.... [', nm_vrbl, ']\n')
    x <- mdl_data[[nm_vrbl]]
    y <- mdl_data[[mtrc]]
    nm <- mdl_data[['scinm']]
    genus <- as.character(mdl_data[['genus']])
    family <- as.character(mdl_data[['family']])
    order <- as.character(mdl_data[['order']])
    data <- data.frame(y, x, nm, genus, family, order)
    data[['x']][data[['x']] == -Inf] <- NA
    data[['x']][data[['x']] == Inf] <- NA
    data <- na.omit(data)
    if(nrow(data) < 50) {
      next
    }
    # select NULL model
    ms <- list()
    ms[[1]] <- lm(y~1, data=data)
    rndm_effcts <- c('(1|genus)', '(1|family)',
                     '(1|order)', '(1|family/genus)',
                     '(1|order/family)', '(1|order/genus)',
                     '(x|genus)', '(x|family)',
                     '(x|order)', '(x|family/genus)',
                     '(x|order/family)', '(x|order/genus)')
    drp_bool <- rep(NA, length(rndm_effcts) + 1)
    drp_bool[1] <- TRUE
    ms <- vector('list', length=length(rndm_effcts) + 1)
    ms[[1]] <- lm(y~1, data=data)
    for(i in 1:length(rndm_effcts)) {
      frml <- paste0('y~1+', rndm_effcts[[i]])
      options(warn=2)
      m <- try(lmer(frml, data=data, REML=FALSE),
               silent=TRUE)
      drp_bool[i+1] <- is(m)[[1]] != 'try-error'
      ms[[i+1]] <- m
    }
    nulli <- which(drp_bool)[which.min(sapply(ms[drp_bool], AIC))]
    m0 <- ms[[nulli]]
    rm(ms)
    if(nulli == 1) {
      # no need for random effects
      m1 <- lm(y~x, data=data)
    } else {
      frml <- paste0('y~x+', rndm_effcts[[nulli-1]])
      m1 <- lmer(frml, data=data, REML=FALSE)
    }
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
    if(!is.numeric(data$y)) {
      int <- slp <- NA
    } else {
      int <- sm1$coefficients[1,1]
      slp <- sm1$coefficients[2,1]
    }
    tmp <- data.frame(grp=grp, n=nrow(data), x=mtrc, y=nm_vrbl,
                      frmla=frmla, int=int, slp=slp, NULL_AIC=aics[1],
                      AIC=aics[2], p=p)
    res <- rbind(res, tmp)
  }
  res[-1, ]
}
