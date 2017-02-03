# ANALYSIS STAGE

# LIBS
library(ggplot2)
library(mgcv)
library(nlme)
library(gridExtra)
library(tree)
library(Amelia)
source(file.path("tools", "plot_tools.R"))

# DIRS
input_dir <- "5_pack"
results_path <- 'results'
input_file <- file.path(input_dir, "res.RData")

# LOAD
load(input_file)

# CHECK FOR VARIABLE NORMALITY
nrml_vrbls <- NULL
for(vrbl in colnames(epi)[14:ncol(epi)]) {
  if(!is.numeric(epi[[vrbl]])) {
    next
  }
  hist(epi[[vrbl]], main=vrbl)
  rspns <- readline(prompt="Log (y/n)?: ")
  bool <- grepl('y', rspns, ignore.case=FALSE)
  if(bool) {
    new_vrbl <- paste0(vrbl, '_log')
    epi[[new_vrbl]] <- log(epi[[vrbl]])
    nrml_vrbls <- c(nrml_vrbls, new_vrbl)
  } else {
    nrml_vrbls <- c(nrml_vrbls, vrbl)
  }
}

# LOOP THROUGH EXP VARS
snfcnt_vrbls <- NULL
for(vrbl in nrml_vrbls) {
  exp_vrbl <- epi[ ,vrbl]
  pull <- !is.na(exp_vrbl)
  exp_vrbl <- exp_vrbl[pull]
  pepi_vrbl <- epi[pull,'pepi']
  m1 <- cor.test(pepi_vrbl, exp_vrbl)
  m2 <- suppressWarnings(cor.test(pepi_vrbl, exp_vrbl, method='spearman'))
  if(m1$p.value < 0.05 | m2$p.value < 0.05) {
    snfcnt_vrbls <- c(snfcnt_vrbls, vrbl)
  }
}

# BUILD AND ASSESS MODELS

# BIRD MODELLING
brd_data <- epi[epi[['txnmcgrp']] == 'birds', ]
brd_data <- brd_data[ ,c('pepi', snfcnt_vrbls)]
brd_data$cate <- NA
brd_data[['nhbbts_log']][brd_data[['nhbbts_log']] == -Inf] <- NA
# remove columns with too few obs
pull <- colSums(!is.na(brd_data)) > 250
brd_data <- brd_data[ ,pull]
brd_data <- na.omit(brd_data)
# model
m <- tree(pepi~., data=brd_data)
plot(m)
text(m)
m <- lm(pepi~., data=brd_data)
summary(m)  # nothing stands out
# plot
for(vrbl in colnames(brd_data)) {
  plot(brd_data$pepi, brd_data[,vrbl], main=vrbl)
}
# test for non-linearity
for(vrbl in colnames(brd_data)) {
  m <- gam(brd_data$pepi~s(brd_data[,vrbl]))
  plot(m, main=vrbl)
}
# model possible non-linears
m <- gam(pepi~s(mass_g_log)+s(nhbbts_log)+s(ncntrs_log)+
           s(Clutch_size_log)+s(iucn_range_log), data=brd_data)
summary(m) # no relationship

m <- lm(pepi~I(mass_g_log^2), data=brd_data)

plot(brd_data$pepi, brd_data$mass_g_log)
plot(epi$pepi, epi$mass_g_log)

m <- gam(pepi~s(mass_g_log), data=epi)
summary(m)
m <- gamm(pepi~s(mass_g_log), data=epi)
summary(m$gam)
acf(resid(m$lme, type = "normalized"))

library(lmtest)
dwtest(pepi ~ mass_g_log, data=epi)

plot(m)
m <- glm(pepi~mass_g_log+I(mass_g_log^2)+
           I(mass_g_log^3)+I(mass_g_log^4), data=epi)
summary(m)


# CATEGORY OF EXTINCTION
cate_data <- epi[,c('epi', 'pepi', 'txnmcgrp', "cate")]
cate_data <- cate_data[!is.na(cate_data[['cate']]), ]
hist(log(cate_data$cate))
m1 <- glm(cate~pepi, data=cate_data, family='poisson')
summary(m1)
m2 <- lm(pepi~cate, data=cate_data)
summary(m2)
plot(pepi~cate, data=cate_data)
abline(m2)
m3 <- cor.test(cate_data$pepi, cate_data$cate, method='spearman')
print(m3)
m4 <- cor.test(cate_data$pepi, cate_data$cate, method='pearson')
print(m4)
# epi
m1 <- glm(cate~epi, data=cate_data, family='poisson')
summary(m1)
m2 <- lm(epi~cate, data=cate_data)
summary(m2) # very very weak
plot(epi~cate, data=cate_data)
abline(m2)
m3 <- cor.test(cate_data$epi, cate_data$cate, method='spearman')
print(m3)
m4 <- cor.test(cate_data$epi, cate_data$cate, method='pearson')
print(m4)
# taxonomic groups
for(grp in unique(cate_data$txnmcgrp)) {
  pull <- cate_data$txnmcgrp == grp
  m <- cor.test(cate_data$pepi[pull], cate_data$cate[pull],
                method='spearman')
  if(m$p.value < 0.05) {
    cat('... [', grp, '] significant:\n')
    print(m)
  }
}
# BIG differences between the taxonomic groups
m1 <- glm(cate~pepi, data=cate_data)
m2 <- glm(cate~pepi+txnmcgrp, data=cate_data)
m3 <- glm(cate~pepi+pepi:txnmcgrp, data=cate_data)
m4 <- glm(cate~pepi+txnmcgrp+pepi:txnmcgrp, data=cate_data)
anova(m1, m2, m3, m4)
AIC(m1, m2, m3, m4)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
cate_data$plant_animal <- ifelse(cate_data$txnmcgrp == 'plants',
                                 'plant', 'animal')
m5 <- glm(cate~pepi+plant_animal, data=cate_data)
m6 <- glm(cate~pepi+pepi:plant_animal, data=cate_data)
m7 <- glm(cate~pepi+plant_animal+pepi:plant_animal, data=cate_data)
AIC(m1, m2, m3, m4, m5, m6, m7)
anova(m1, m2, m3, m4, m5, m6, m7)
# cannot say that the different trajectories among the taxonomic groups
# impacts the overall result: very weak negative correlation
pull <- cate_data$cate != 0
m <- lm(pepi~cate, data=cate_data[pull,])
summary(m)
# significance disappears if 0 values are removed
t.test(cate_data$pepi[cate_data$cate > 0],
       cate_data$pepi[cate_data$cate == 0])
t.test(cate_data$epi[cate_data$cate > 0],
       cate_data$epi[cate_data$cate == 0])
# clades at risk of extinction are more likely to be living fossils
cate_data$at_risk <- cate_data$cate > 0
ggplot(cate_data, aes(x=pepi, group=at_risk, colour=at_risk, fill=at_risk)) +
  geom_density(alpha=.75)
# t test confirms linear modelling


# IUCN MODEL
iucn_data <- epi[, c('epi', 'pepi', 'txnmcgrp', 'nhbbts_log', 'ncntrs_log')]
iucn_data$nhbbts_log[iucn_data$nhbbts_log == -Inf] <- NA
iucn_data$ncntrs_log[iucn_data$ncntrs_log == -Inf] <- NA
pull <- !is.na(iucn_data$nhbbts_log) & !is.na(iucn_data$ncntrs_log)
iucn_data <- iucn_data[pull, ]
m1 <- lm(pepi~nhbbts_log*ncntrs_log, data=iucn_data)
m2 <- lm(pepi~nhbbts_log+ncntrs_log, data=iucn_data)
m3 <- lm(pepi~nhbbts_log, data=iucn_data)
m4 <- lm(pepi~ncntrs_log, data=iucn_data)
anova(m1, m2, m3, m4)  # not strong evidence for N countries
summary(m3)
plot(pepi~nhbbts_log, data=iucn_data)
abline(m3)
plot(m3) # doesn't look too bad
cor.test(iucn_data$pepi, iucn_data$nhbbts_log)
cor.test(iucn_data$epi, iucn_data$nhbbts_log)  # similar correlation for EPI
# what's the taxonomic diverisity of this data?
table(iucn_data$txnmcgrp)
# do we find the same pattern for all group?
for(grp in unique(iucn_data$txnmcgrp)) {
  pull <- iucn_data$txnmcgrp == grp
  m <- cor.test(iucn_data$pepi[pull], iucn_data$nhbbts_log[pull])
  if(m$p.value < 0.05) {
    cat('... [', grp, '] significant:\n')
    print(m)
  }
}
# purely driven by mammals, birds essentially show no correlation

# MAMMAL MODELLING
mml_data <- epi[epi[['txnmcgrp']] == 'mammals', ]
pepi <- mml_data[['pepi']]
mml_data <- mml_data[ ,-1*1:14]
mml_data[['pepi']] <- pepi
mml_data[['nhbbts_log']][mml_data[['nhbbts_log']] == -Inf] <- NA
# remove columns with too few obs
pull <- colSums(!is.na(mml_data)) > 500
mml_data <- mml_data[ ,pull]
mml_data <- na.omit(mml_data)
# explore for possible explanatory variables
mdl <- tree(pepi~., data=mml_data)
plot(mdl)
text(mdl)
m1 <- lm(pepi~., data=mml_data)
summary(m1)
m2 <- lm(pepi~X10.2_SocialGrpSize_log*X22.1_HomeRange_km2_log*X23.1_SexualMaturityAge_d*nhbbts,
         data=mml_data)
summary(m2)
m3 <- lm(pepi~X10.2_SocialGrpSize_log+X22.1_HomeRange_km2_log+X23.1_SexualMaturityAge_d+nhbbts,
         data=mml_data)
summary(m3)
m4 <- lm(pepi~X10.2_SocialGrpSize_log+X23.1_SexualMaturityAge_d+nhbbts,
         data=mml_data)
summary(m4)
m5 <- lm(pepi~X10.2_SocialGrpSize_log+nhbbts,
         data=mml_data)
summary(m5)
m6 <- lm(pepi~X10.2_SocialGrpSize_log, data=mml_data)
summary(m6)
# reconstruct data frame with possible significants
mml_data <- epi[epi[['txnmcgrp']] == 'mammals', ]
mml_data <- mml_data[ ,c('pepi', 'X10.2_SocialGrpSize_log', 'X22.1_HomeRange_km2_log',
                         'X23.1_SexualMaturityAge_d')]
mml_data <- na.omit(mml_data)
m1 <- lm(pepi~., data=mml_data)
summary(m1)

# gams
library(mgcv)
gm_mdl <- gam(pepi~s(log_bslmtblcrt)+s(log_mxlngvty)+s(log_sxlmtrtyag), data=mml_data)
plot(gm_mdl)
# non-linear relationship for basal metabolic rate?
m3 <- lm(pepi~log_sclgrpsz*X15.1_LitterSize+I(log_bslmtblcrt^2), data=mml_data)
summary(m3)


m4 <- lm(pepi~log_sclgrpsz+X15.1_LitterSize, data=mml_data)
m5 <- lm(pepi~X15.1_LitterSize, data=mml_data)
m6 <- lm(pepi~log_sclgrpsz, data=mml_data)
anova(m4, m5)
plot(m5)
# remove outliers
m7 <- lm(pepi~X15.1_LitterSize, data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m7) # no longer significant


m8 <- lm(pepi~log_sclgrpsz+X15.1_LitterSize, data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m8)
m9 <- lm(pepi~log_sclgrpsz*X15.1_LitterSize, data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m9)
m10 <- lm(pepi~X15.1_LitterSize, data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m10)
m11 <- lm(pepi~log_sclgrpsz, data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m11)
anova(m8, m10, m11)


m12 <- lm(pepi~., data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m12)

m13 <- lm(pepi~X15.1_LitterSize+X28.2_Temp_Mean_01degC+log_sclgrpsz,
          data=mml_data[!rownames(mml_data) %in% c('21737', '938', '71'),])
summary(m13)

plot(m8)



# SOCIAL GROUP SIZE
sclgrp_data <- epi[,c('epi', 'pepi', "X10.2_SocialGrpSize_log")]
sclgrp_data <- sclgrp_data[!is.na(sclgrp_data[['X10.2_SocialGrpSize_log']]), ]
m <- lm(pepi~X10.2_SocialGrpSize_log, data=sclgrp_data)
summary(m)
plot(pepi~X10.2_SocialGrpSize_log, sclgrp_data)
abline(m)
m <- lm(epi~X10.2_SocialGrpSize_log, data=sclgrp_data)
summary(m)
plot(epi~X10.2_SocialGrpSize_log, sclgrp_data)
abline(m)
# try without 0s
pull <- sclgrp_data$X10.2_SocialGrpSize_log != 0
m <- lm(pepi~X10.2_SocialGrpSize_log, data=sclgrp_data[pull, ])
summary(m)  # still holds
# check model
m <- lm(pepi~X10.2_SocialGrpSize_log, data=sclgrp_data)
summary(m)
plot(m)
# re-model without potential outliers
pull <- !rownames(sclgrp_data) %in% c('71', '21737', '284')
m <- lm(pepi~X10.2_SocialGrpSize_log, data=sclgrp_data[pull, ])
summary(m)  # slightly better model, 0.18
