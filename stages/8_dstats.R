# DESCRIPTIVE STATS

# START
cat(paste0('\nStage `dstats` started at [', Sys.time(), ']\n'))

# LIBS
source(file.path('tools', 'dstats_tools.R'))

# DIRS
input_dir <- "7_pack"
rslts_dir <- 'results'
if(!file.exists(rslts_dir)) {
  dir.create(rslts_dir)
}
input_file <- file.path(input_dir, "res.RData")

# LOAD
load(input_file)

# CORRELATIONS
# all groups
cor_res <- getCorrs(mdl_data=epi, mtrc='pepi', vrbls=all_vrbls)
# epi data
mdl_data <- epi[!is.na(epi$epi), ]
epi_res <- getCorrs(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                            grp='EPI Dataset')
cor_res <- rbind(cor_res, epi_res)
epi_res <- getCorrs(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                            grp='EPI Dataset')
cor_res <- rbind(cor_res, epi_res)
rm(epi_res)
# each of the txnmcgrps
for(grp in unique(epi[['txnmcgrp']])) {
  mdl_data <- epi[as.character(epi[['txnmcgrp']]) == grp, ]
  grp_res <- getCorrs(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                      grp=grp)
  cor_res <- rbind(cor_res, grp_res)
  grp_res <- getCorrs(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                      grp=grp)
  cor_res <- rbind(cor_res, grp_res)
}

# CATEGORICAL MEANS
# all groups
cat_res <- getCatMeans(mdl_data=epi, mtrc='pepi', vrbls=all_vrbls)
# epi data
mdl_data <- epi[!is.na(epi$epi), ]
epi_res <- getCatMeans(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                    grp='EPI Dataset')
cat_res <- rbind(cat_res, epi_res)
epi_res <- getCatMeans(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                    grp='EPI Dataset')
cat_res <- rbind(cat_res, epi_res)
rm(epi_res)
# each of the txnmcgrps
for(grp in unique(epi[['txnmcgrp']])) {
  mdl_data <- epi[as.character(epi[['txnmcgrp']]) == grp, ]
  grp_res <- getCatMeans(mdl_data=mdl_data, mtrc='pepi', vrbls=all_vrbls,
                         grp=grp)
  cat_res <- rbind(cat_res, grp_res)
  grp_res <- getCatMeans(mdl_data=mdl_data, mtrc='epi', vrbls=all_vrbls,
                         grp=grp)
  cat_res <- rbind(cat_res, grp_res)
}

# SAVE
write.csv(cor_res, file=file.path(rslts_dir, 'correlations_full.csv'))
write.csv(cor_res[sum(abs(cor_res$pearson) > .1), ],
          file=file.path(rslts_dir, 'correlations_prt.csv'))
write.csv(cat_res, file=file.path(rslts_dir, 'differences.csv'))

# END
cat(paste0('\nStage `dstats` finished at [', Sys.time(), ']\n'))