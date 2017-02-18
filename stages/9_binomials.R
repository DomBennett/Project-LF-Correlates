# LINEAR MIXED EFFECTS MODELLELING

# START
cat(paste0('\nStage `binomials` started at [', Sys.time(), ']\n'))

# LIBS
library(lme4)
source(file.path('tools', 'binomial_tools.R'))

# DIRS
input_dir <- "7_pack"
rslts_dir <- 'results'
if(!file.exists(rslts_dir)) {
  dir.create(rslts_dir)
}
input_file <- file.path(input_dir, "res.RData")
hbbts_file <- file.path('3_habitats', "res.RData")

# LOAD
load(input_file)
load(hbbts_file)

# VRBLS
epi$cate_bn <- epi$cate > 0
epi$volancy_bn <- as.character(epi$volancy) == 'volant'
epi$volancy_bn <- as.numeric(epi$volancy_bn)
hbbts <- colnames(hbbts_epi)[!colnames(hbbts_epi) %in% colnames(epi)]
vrbls <- c('volancy_bn', 'cate_bn', 'terrestrial', 'marine')

# MERGE
i <- match(epi$txid, hbbts_epi$txid)
j <- colnames(hbbts_epi)[!colnames(hbbts_epi) %in% colnames(epi)]
epi <- cbind(epi, hbbts_epi[i, j])
rm(hbbts_epi)

# SPP DATA
epi <- epi[epi[['n']] == 1, ]

# MDL DATASETS
trrstrl <- epi[epi$terrestrial == 1, ]
marine <- epi[epi$marine == 1, ]

# LOOPS
cat('Looping through binomial tests....\n')
# all groups
mdl_res <- loopThroughTests(mdl_data=epi, mtrc='pepi', vrbls=vrbls)
# EPI dataset
mdl_data <- epi[!is.na(epi$epi), ]
pepi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                             grp='EPI dataset')
epi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='EPI dataset')
mdl_res <- rbind(mdl_res, pepi_res, epi_res)
# birds
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'birds', ]
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
# mammals
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'mammals', ]
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
cat('Done.\n')

# LOOPS
cat('Looping through terrestrial tests....\n')
# all groups
mdl_res <- loopThroughTests(mdl_data=trrstrl, mtrc='pepi', vrbls=hbbts)
# EPI dataset
mdl_data <- trrstrl[!is.na(trrstrl$epi), ]
pepi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                             grp='EPI dataset (terrestrial)')
epi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='EPI dataset (terrestrial)')
mdl_res <- rbind(mdl_res, pepi_res, epi_res)
# birds
mdl_data <- trrstrl[as.character(trrstrl[['txnmcgrp']]) == 'birds', ]
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                            grp='Aves (terrestrial)')
mdl_res <- rbind(mdl_res, avs_res)
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='Aves (terrestrial)')
mdl_res <- rbind(mdl_res, avs_res)
# mammals
mdl_data <- trrstrl[as.character(trrstrl[['txnmcgrp']]) == 'mammals', ]
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                            grp='Mammalia (terrestrial)')
mdl_res <- rbind(mdl_res, mml_res)
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='Mammalia (terrestrial)')
mdl_res <- rbind(mdl_res, mml_res)
cat('Done.\n')

cat('Looping through marine tests....\n')
# all groups
mdl_res <- loopThroughTests(mdl_data=marine, mtrc='pepi', vrbls=hbbts)
# EPI dataset
mdl_data <- marine[!is.na(marine$epi), ]
pepi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                             grp='EPI dataset (marine)')
epi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='EPI dataset (marine)')
mdl_res <- rbind(mdl_res, pepi_res, epi_res)
# birds
mdl_data <- marine[as.character(marine[['txnmcgrp']]) == 'birds', ]
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                            grp='Aves (marine)')
mdl_res <- rbind(mdl_res, avs_res)
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='Aves (marine)')
mdl_res <- rbind(mdl_res, avs_res)
# mammals
mdl_data <- marine[as.character(marine[['txnmcgrp']]) == 'mammals', ]
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=hbbts,
                            grp='Mammalia (marine)')
mdl_res <- rbind(mdl_res, mml_res)
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=hbbts,
                            grp='Mammalia (marine)')
mdl_res <- rbind(mdl_res, mml_res)
cat('Done.\n')

# SAVE
write.csv(mdl_res, file=file.path(rslts_dir, 'binomials_fits.csv'))

# END
cat(paste0('\nStage `binomials` finished at [', Sys.time(), ']\n'))