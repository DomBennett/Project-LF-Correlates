
load("epi_mammals.RData")

library(nlme)

names(epi)

# TEMP
hist(epi[['Temp_Mean_01degC_mean']])  # more normal than anything
pull <- !is.na(epi[['Temp_Mean_01degC_mean']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~Temp_Mean_01degC_mean, data=epi[pull, ])
m3 <- lm(pepi~Temp_Mean_01degC_mean+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(Temp_Mean_01degC_mean~pepi, data=epi)  # not significant

# POPULATION GROUP SIZE
hist(epi[['PopulationGrpSize_med']])  # COUNT
# TODO
epi$pgs_log <- log(epi[['PopulationGrpSize_med']])
hist(epi[['pgs_log']])
pull <- !is.na(epi[['ml_log']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~ml_log, data=epi[pull, ])
m3 <- lm(pepi~ml_log+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(ml_log~pepi, data=epi)

# DIET BREADTH
hist(epi[['DietBreadth_med']])  # poisson, use count
epi$ls_log <- log(epi[['LitterSize_med']])
# TODO
hist(epi[['ls_log']])
pull <- !is.na(epi[['ml_log']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~ml_log, data=epi[pull, ])
m3 <- lm(pepi~ml_log+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(ml_log~pepi, data=epi)

# LITTER SIZE
hist(epi[['LitterSize_med']])  # poisson, use count
epi$ls_log <- log(epi[['LitterSize_med']])
# TODO
hist(epi[['ls_log']])
pull <- !is.na(epi[['ml_log']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~ml_log, data=epi[pull, ])
m3 <- lm(pepi~ml_log+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(ml_log~pepi, data=epi)

# MAX LONGEVITY
hist(epi[['MaxLongevity_m_med']])  # logistic, use med
epi$ml_log <- log(epi[['MaxLongevity_m_med']])
hist(epi[['ml_log']])
pull <- !is.na(epi[['ml_log']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~ml_log, data=epi[pull, ])
m3 <- lm(pepi~ml_log+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(ml_log~pepi, data=epi)  # not significant


# SOCIAL GROUP SIZE
hist(epi[["SocialGrpSize_mean"]])  # poisson, use mean
epi$sgs_count <- round(epi[["SocialGrpSize_mean"]])
pull <- !is.na(epi[['sgs_count']])
m1 <- glm(sgs_count ~ 1, data=epi[pull, ], family='poisson')
m2 <- glm(sgs_count ~ pepi, data=epi[pull, ], family='poisson')
m3 <- glm(sgs_count ~ pepi + sstr_pepi, data=epi[pull, ], family = 'poisson')
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)  # m3 is best, but sgs has very small impact
plot(SocialGrpSize_mean~pepi, data=epi)


# SEX MATURITY
hist(epi[['SexualMaturityAge_d_med']])  # logistic, use med
epi$sm_log <- log(epi[['SexualMaturityAge_d_med']])
hist(epi[['sm_log']])
pull <- !is.na(epi[['sm_log']])
m1 <- lm(pepi~sstr_pepi, data=epi[pull, ])
m2 <- lm(pepi~sm_log, data=epi[pull, ])
m3 <- lm(pepi~sm_log+sstr_pepi, data=epi[pull, ])
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)
plot(sm_log~pepi, data=epi)


# BODY MASS
epi$bm_log <- log(epi$AdultBodyMass_g_med)
epi$n_log <- log(epi$n)
ss_epi <- epi[epi[['n']] == 1, ]
epi <- epi[!is.na(epi[['bm_log']]) & !is.na(epi[['sstr_pepi']]), ]


m1 <- lm(pepi~sstr_pepi, data=epi)
m2 <- lm(pepi~bm_log, data=epi)
m3 <- lm(pepi~bm_log+sstr_pepi, data=epi)
summary(m1)
summary(m2)
summary(m3)
anova(m2, m3)
anova(m1, m3)


ggplot(epi, aes(x=pepi, y=bm_log)) + geom_point() +stat_smooth(method="gam")
ggplot(epi, aes(x=n_log, y=bm_log)) +geom_point() + stat_smooth(method="gam")
ggplot(ss_epi, aes(x=pepi, y=bm_log)) + geom_point() +stat_smooth(method="gam")


which('Monotremata' == epi[['nm']])
epi[3584, ]



cor.test(epi[['pepi']], epi[['bm_log']], method="spearman")
cor.test(epi[['pepi']], epi[['bm_log']], method="pearson")
cor.test(epi[['pepi']], epi[['bm_log']], method="kendall")



m1 <- lm(log(epi[[o]]) ~ epi[['pepi']])
summary(m1)
plot(epi[['pepi']], log(epi[[o]]))
abline(m1)

library(mgcv)
m2 <- gam(log(epi[[o]]) ~ epi[['pepi']])
summary(m2)

library(ggplot2)

epi$bm_log <- log(epi[['X5.1_AdultBodyMass_g']])
p <- ggplot(epi, aes(x=pepi, y=bm_log)) +
  geom_point()
p + stat_smooth(method = "gam")

m3 <- loess(bm_log~pepi, data=epi)
summary(m3)



metrics$node.label <- sub ('_', ' ', metrics$node.label)
data$lfi <- metrics$lfi[match (data$MSW93_Binomial, metrics$node.label)]
data <- data[!is.na (data$lfi),]
data <- data[!is.na (data$X26.4_GR_MRLat_dd),]
data <- data[!is.na (data$X26.7_GR_MRLong_dd),]
data <- data[order (data$lfi), ]
data$tree_binomials <- gsub(" ", "_", data$MSW93_Binomial)
lfi <- data$lfi
lat <- data$X26.4_GR_MRLat_dd
long <- data$X26.7_GR_MRLong_dd
range <- data$X22.1_HomeRange_km2
plot(log(data$X22.1_HomeRange_km2), data$lfi)
abline(lm (lfi~range))

names(data)


# load tree for nlme
library(ape)
library(nlme)
tree <- read.tree('0_data/raw/bininda.txt')
# TODO: do this for all trait values
model.data <- data[!is.na(data$X22.1_HomeRange_km2), ]
rownames(model.data) <- model.data$tree_binomials
to.drop <- tree$tip.label[!tree$tip.label %in% model.data$tree_binomials]
model.tree <- drop.tip(tree, to.drop)
model.data$home.range.logged <- log(model.data$X22.1_HomeRange_km2)
hist(model.data$home.range.logged)
model <- gls(home.range.logged ~ lfi, data=model.data, method="ML",
             correlation=corPagel(value=1, phy=model.tree, fixed=TRUE))
summary(model)
plot(model)

phyloGLS <- function(data, cname) {
  model.data <- data[!is.na(data[[cname]]), ]
  rownames(model.data) <- model.data$tree_binomials
  to.drop <- tree$tip.label[!tree$tip.label %in% model.data$tree_binomials]
  model.tree <- drop.tip(tree, to.drop)
  model.data[[cname]] <- log(model.data[[cname]])
  hist(model.data[[cname]])
  frml <- as.formula(paste0(cname, " ~ lfi"))
  model <- gls(frml, data=model.data, method="ML",
               correlation=corPagel(value=1, phy=model.tree, fixed=TRUE))
  model
}


for(i in 1:ncol(data)) {
  if(is(data[ ,i], 'vector')) {
    #plot(data[ ,i], data$lfi, main=colnames(data)[i])
    #plot(log(data[ ,i]), data$lfi, main=paste0(colnames(data)[i], 'logged'))
    res <- phyloGLS(data, colnames(data)[i])
    if(res['p.value'][[1]] < 0.05) {
      cat('-------------------------\n')
      cat(colnames(data)[i], ':\ncor =', res$estimate[[1]],
          '\np.value =', res['p.value'][[1]],'\n', sep=" ")
    } else {
      res <- phyloGLS(data, colnames(data)[i])
      if(!is.na(res['p.value'][[1]]) && res['p.value'][[1]] < 0.05) {
        cat('-------------------------\n')
        cat(colnames(data)[i], '(logged):\ncor =', res$estimate[[1]],
            '\np.value =', res['p.value'][[1]],'\n', sep=" ")
      }
    }
  }
}

# repeat but for low EPI things
low_data <- data[data$lfi > 0.5, ]
for(i in 1:ncol(low_data)) {
  if(is(low_data[ ,i], 'vector') && sum(!is.na(low_data[,i])) > 10) {
    res <- cor.test(low_data[,i], low_data$lfi)
    if(res['p.value'][[1]] < 0.05) {
      cat('-------------------------\n')
      cat(colnames(low_data)[i], ':\ncor =', res$estimate[[1]],
          '\np.value =', res['p.value'][[1]],'\n', sep=" ")
    } else {
      res <- cor.test(log(low_data[,i]), low_data$lfi)
      if(!is.na(res['p.value'][[1]]) && res['p.value'][[1]] < 0.05) {
        cat('-------------------------\n')
        cat(colnames(low_data)[i], '(logged):\ncor =', res$estimate[[1]],
            '\np.value =', res['p.value'][[1]],'\n', sep=" ")
      }
    }
  }
}

plot(log(low_data[,"X5.1_AdultBodyMass_g"]), low_data$lfi)
cor.test(log(low_data[,"X5.1_AdultBodyMass_g"]), low_data$lfi)
plot(low_data[,"X15.1_LitterSize"], low_data$lfi)
plot(log(low_data[,"X26.1_GR_Area_km2"]), low_data$lfi)
which(data$lfi > 1.4)
data$MSW93_Binomial[3878]



grid_size <- 10
lon <- seq(-180, 180-grid_size, grid_size)
lat <- seq(-60, 90-grid_size, grid_size)
mapobj <- expand.grid(lat=lat, lon=lon)
mapobj$lfi <- NA
for(i in 1:nrow(mapobj)) {
  bool <- mapobj$lat[i] < data$X26.4_GR_MRLat_dd &
    (mapobj$lat[i] + grid_size) > data$X26.4_GR_MRLat_dd &
    mapobj$lon[i] < data$X26.7_GR_MRLong_dd &
    (mapobj$lon[i] + grid_size) > data$X26.7_GR_MRLong_dd
  data[bool, ]
  if(sum(bool) == 0) {
    mapobj[i, 'lfi'] <- NA
  } else {
    mapobj[i, 'lfi'] <- mean(data$lfi[bool], na.rm=TRUE)
  }
}
mapobj <- na.omit(mapobj)
mapobj$lat <- mapobj$lat + (grid_size/2)
mapobj$lon <- mapobj$lon + (grid_size/2)

worldmap <- map_data(map = "world", ylim=c(-60,90))
worldmap <- geom_path(aes(x=long, y=lat, group=group),
                      data = worldmap)
map <- ggplot() + worldmap +
  geom_tile(aes(x=lon, y=lat, fill=lfi), size=3, alpha=0.9, data=mapobj) +
  theme_bw() + ylab("Latitude") + xlab("Longitude") + scale_fill_gradient2(low = 'blue', mid = 'cornflowerblue',
                                                                             high = 'red')
map

# New Zealand comes out -- but that's cos not many mammals live there.

pdf("map.pdf", height = 6, width = 10.5)
print (map)
dev.off()

