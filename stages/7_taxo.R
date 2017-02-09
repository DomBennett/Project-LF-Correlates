# LINEAR MIXED EFFECTS MODELLELING

# START
cat(paste0('\nStage `taxo` started at [', Sys.time(), ']\n'))

# LIBS
library(lme4)
source(file.path('tools', 'taxo_tools.R'))

# DIRS
input_dir <- "6_vrbls"
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

# # DETERMINE OUTPUT ORDER
# mdl_res <- mdl_res[order(abs(mdl_res[['slp']]), decreasing=TRUE), ]
# ys <- unique(mdl_res[['y']])
# nstrs <- rep(NA, length(ys))
# names(nstrs) <- ys
# for(y in ys) {
#   strs <- mdl_res[mdl_res[['y']] == y, 'p']
#   nstrs[y] <- length(strsplit(paste(strs, collapse=''), split="\\*")[[1]]) - 1
# }
# nstrs <- sort(nstrs, decreasing=TRUE)
# ys <- names(nstrs)
# ordr <- NULL
# for(y in ys) {
#   ordr <- c(ordr, which(mdl_res[['y']] == y))
# }
# mdl_res <- mdl_res[ordr,]


# SAVE
write.csv(mdl_res, file=file.path(rslts_dir, 'lmem_fits.csv'))

# END
cat(paste0('\nStage `taxo` finished at [', Sys.time(), ']\n'))