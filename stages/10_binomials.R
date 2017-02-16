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
vrbls <- c(colnames(hbbts_epi)[!colnames(hbbts_epi) %in% colnames(epi)],
           'volancy', 'cate_bn')

# MERGE
i <- match(epi$txid, hbbts_epi$txid)
j <- colnames(hbbts_epi)[!colnames(hbbts_epi) %in% colnames(epi)]
epi <- cbind(epi, hbbts_epi[i, j])
rm(hbbts_epi)

# SPP DATA
epi <- epi[epi[['n']] == 1, ]

# LOOPS
cat('Looping through different tests....\n')
# all groups
mdl_res <- loopThroughTests(mdl_data=epi, mtrc='pepi', vrbls=vrbls)
# EPI dataset
mdl_data <- epi[!is.na(epi$epi), ]
pepi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                             grp='EPI dataset')
epi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='EPI dataset')
mdl_res <- rbind(mdl_res, pepi_res, epi_res)
rm(epi_res, pepi_res)
# birds
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'birds', ]
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
rm(avs_res)
# mammals
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'mammals', ]
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls[1:2],
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
rm(mmL_res)
cat('Done.\n')

# SAVE
write.csv(mdl_res, file=file.path(rslts_dir, 'binomials_fits.csv'))

# END
cat(paste0('\nStage `binomials` finished at [', Sys.time(), ']\n'))