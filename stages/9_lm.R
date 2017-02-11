# LINEAR REGRESSION MODELLELING

# START
cat(paste0('\nStage `lm` started at [', Sys.time(), ']\n'))

# LIBS
library(lme4)
source(file.path('tools', 'lm_tools.R'))

# DIRS
input_dir <- "8_vrbls"
rslts_dir <- 'results'
if(!file.exists(rslts_dir)) {
  dir.create(rslts_dir)
}
input_file <- file.path(input_dir, "res.RData")

# LOAD
load(input_file)

# DROP NON-CONTS VRBLS
all_vrbls <- all_vrbls[!all_vrbls %in% c("Volancy", "Fossoriality",
                                         "Foraging environment", "Daily activity")]

# LOOPS
cat('Looping through different tests....\n')
# all groups
mdl_res <- loopThroughTests(mdl_data=epi, mtrc='pepi', vrbls=
                              c("Category of extinction (log)", "No. habitats (log)", "No. countries (log)" ))
epi_res <- loopThroughTests(mdl_data=epi, mtrc='epi', vrbls=
                              c("Category of extinction (log)", "No. habitats (log)", "No. countries (log)" ),
                            grp='Mammalia & Aves')
mdl_res <- rbind(mdl_res, epi_res)
rm(epi_res)
# mammals
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'mammals', ]
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
mml_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                            grp='Mammalia')
mdl_res <- rbind(mdl_res, mml_res)
rm(mml_res)
# birds
mdl_data <- epi[as.character(epi[['txnmcgrp']]) == 'birds', ]
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
avs_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                            grp='Aves')
mdl_res <- rbind(mdl_res, avs_res)
rm(avs_res)
cat('Done.\n')

# SAVE
write.csv(mdl_res, file=file.path(rslts_dir, 'lm_fits.csv'))

# END
cat(paste0('\nStage `lm` finished at [', Sys.time(), ']\n'))