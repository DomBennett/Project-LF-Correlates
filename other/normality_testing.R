# IS PEPI NORMAL?

# LIBS
library(fitdistrplus)
library(logspline)
library(e1071)
library(ggplot2)

# INPUT
epi_file <- file.path('1_download', 'epi.RData')
load(epi_file)

# PLOT
hist(epi[['pepi']])  # skewed, this is due to uneuql sampling, sorting epi by groups will control for this

# DIFFERENT SUBSETS
pepi_mml <- epi[['pepi']][epi[['txnmcgrp']] == 'mammal']
pepi_mml <- pepi_mml[!is.na(pepi_mml)]
pepi_brd <- epi[['pepi']][epi[['txnmcgrp']] == 'bird']
pepi_brd <- pepi_brd[!is.na(pepi_brd)]
pepi_nn <- epi[['pepi']][epi[['n']] > 1]
pepi_nn <- pepi_nn[!is.na(pepi_nn)]
pepi_nn_mml <- epi[['pepi']][epi[['n']] > 1 &
                               epi[['txnmcgrp']] == 'mammal']
pepi_nn_mml <- pepi_nn_mml[!is.na(pepi_nn_mml)]

# MAMMAL TESTING
ggplot(as.data.frame(pepi_mml), aes(pepi_mml)) + geom_density()  # looks normal
shapiro.test(pepi_mml)  # fails normality test
descdist(pepi_mml, discrete = FALSE)  # but is closest to normal
# kurtosis is higher than 3, leptokurtic
# http://alstatr.blogspot.co.uk/2013/06/measures-of-skewness-and-kurtosis.html
fit_norm <- fitdist(pepi_mml, "norm")
summary(fit_norm)
plot(fit_norm)  # seems mostly normal
# close enough to normal http://www.biostathandbook.com/normality.html

# BIRD TESTING
ggplot(as.data.frame(pepi_brd), aes(pepi_brd)) + geom_density()
shapiro.test(pepi_brd)
descdist(pepi_brd, discrete = FALSE)
fit_norm <- fitdist(pepi_brd, "norm")
summary(fit_norm)
plot(fit_norm)

# ADDITIONAL NORMALITY TESTING
ggplot(as.data.frame(pepi_nn), aes(pepi_nn)) + geom_density()
ggplot(as.data.frame(pepi_nn_mml), aes(pepi_nn_mml)) + geom_density()
