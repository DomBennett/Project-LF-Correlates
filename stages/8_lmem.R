# LINEAR MIXED EFFECTS MODELLELING

# START
cat(paste0('\nStage `lmem` started at [', Sys.time(), ']\n'))

# LIBS
library(lme4)
source(file.path('tools', 'lmem_tools.R'))

# DIRS
input_dir <- "7_pack"
rslts_dir <- 'results'
if(!file.exists(rslts_dir)) {
  dir.create(rslts_dir)
}
input_file <- file.path(input_dir, "res.RData")

# LOAD
load(input_file)

# SPP DATA
epi <- epi[epi[['n']] == 1, ]

# WRITE OUT VARIABLE INFO
nobs <- rep(NA, length(all_vrbls))
names(nobs) <- all_vrbls
for(vrbl in all_vrbls) {
  nobs[vrbl] <- sum(!is.na(epi[[vrbl]]))
}
write.csv(data.frame(all_vrbls, nobs), file=file.path(rslts_dir, 'variable_info.csv'))

# LOOPS
cat('Looping through different tests....\n')
# all groups
vrbls <- c("cate", "nhbbts_log", "ncntrs_log", "volancy",
           "fossoriallity", "foraging_environment",
           "daily_activity", "maximum_lifespan_yr_log",
           "mass_g_log", "BMR_log", "hermit", "lethary", "refugium",
           "specialist", "primitive", "oddness" )
mdl_res <- loopThroughTests(mdl_data=epi, mtrc='pepi', vrbls=vrbls)
mdl_data <- epi[!is.na(epi$epi), ]
pepi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='pepi', vrbls=vrbls,
                            grp='EPI dataset')
epi_res <- loopThroughTests(mdl_data=mdl_data, mtrc='epi', vrbls=vrbls,
                            grp='EPI dataset')
mdl_res <- rbind(mdl_res, pepi_res, epi_res)
rm(epi_res, pepi_res)
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
cat(paste0('\nStage `lmem` finished at [', Sys.time(), ']\n'))