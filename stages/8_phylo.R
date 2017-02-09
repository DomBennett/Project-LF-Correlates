# SIMPLE PHYLOGENETIC LINEAR REGRESSIONS FOR MAMMALS

# START
cat(paste0('\nStage `phylo` started at [', Sys.time(), ']\n'))

# LIBS
library(phytools)
library(nlme)
source(file.path('tools', 'phylo_tools.R'))

# DIRS
input_dir <- "5_pack"
rslts_dir <- 'results'
if(!file.exists(rslts_dir)) {
  dir.create(rslts_dir)
}
input_file <- file.path(input_dir, "res.RData")

# LOAD
load(input_file)
mammal_tree <- read.tree(file.path('0_data', 'mammalia.tre'))

# DROP NON-CONTS VRBLS
all_vrbls <- all_vrbls[!all_vrbls %in% c("volancy", "fossoriallity",
                                         "foraging_environment", "daily_activity")]

# ORGANISE TREE AND DATA
cat('Reading and organising data....\n')
sp_nms <- gsub('_', ' ', mammal_tree$tip.label)
mtchd <- match(sp_nms, as.character(epi$scinm))
mtchd <- mtchd[!is.na(mtchd)]
epi <- epi[mtchd, ]
to_drp <- mammal_tree$tip.label[!sp_nms %in% epi[['scinm']]]
mammal_tree <- drop.tip(mammal_tree, to_drp)
mammal_tree$tip.label <- gsub('_', ' ', mammal_tree$tip.label)
cat('Done.\n')

# TEST FOR PHYLO SIGNAL
cat('Calculating EPI and pEPI phylogenetic signal....\n')
to_drp <- as.character(epi[['scinm']][is.na(epi[['epi']])])
epi_tree <- drop.tip(mammal_tree, to_drp)
mml_epi <- epi[['epi']][!is.na(epi[['epi']])]
names(mml_epi) <- epi[['scinm']][!is.na(epi[['epi']])]
epi_lambda <- phylosig(epi_tree, mml_epi, method='lambda')
cat('.... [', epi_lambda[['lambda']], '] EPI lambda\n', sep='')
epi_k <- phylosig(epi_tree, mml_epi, method='K')
cat('.... [', epi_k, '] EPI K\n', sep='')
mml_pepi <- epi[['pepi']]
names(mml_pepi) <- epi[['scinm']]
pepi_lambda <- phylosig(mammal_tree, mml_pepi, method='lambda')
cat('.... [', pepi_lambda[['lambda']], '] pEPI lambda\n', sep='')
pepi_k <- phylosig(mammal_tree, mml_pepi, method='K')
cat('.... [', pepi_k, '] pEPI K\n', sep='')
cat('Done.\n')

# SELECT VRBLS
cat('Selecting variables....\n')
vrbls <- NULL
for(vrbl in all_vrbls) {
  obs <- epi[[vrbl]]
  if(sum(!is.na(obs)) > 100) {
    vrbls <- c(vrbl, vrbls)
  }
}
cat('Done, selected [', length(vrbls), '].\n', sep='')

# LOOPS
cat('Looping through GLS models....\n')
mml_res <- loopThroughTests('pepi')
mml_res <- rbind(mml_res, loopThroughTests('epi'))
cat('Done.\n')

# SAVE
write.csv(mml_res, file=file.path(rslts_dir, 'phylo_fits.csv'))
save(epi_lambda, epi_k, pepi_lambda, pepi_k, 
     mml_res, file=file.path(rslts_dir, 'phylo_res.RData'))

# END
cat(paste0('\nStage `phylo` finished at [', Sys.time(), ']\n'))
