# TEST IF LIVING FOSSILS SHARE MORE

# START
cat(paste0('\nStage `text permutation` started at [', Sys.time(), ']\n'))

# PARAMETERS
source('parameters.R')

# LIBS
library(doMC)
ncps <- 2
registerDoMC(cores=ncps)

# FUNCTIONS
source(file.path('tools', 'iucn_prmttn_tools.R'))

# DIRS
output_dir <- '6_prmttn'
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}
input_dir <- "5_prmttn_dwnld"
output_file <- file.path(output_dir, "res.RData")
iucn_files <- list.files(input_dir)

# LOOP THROUGH ANALYSIS GROUPS
for(iucn_file in iucn_files) {
  # INPUT
  grp <- sub("\\.RData", "", iucn_file)
  cat('    Working on [', grp, '] ....\n', sep="")
  load(file.path(input_dir, iucn_file))
  gen_res <- matrix(ncol=6, nrow=3)
  colnames(gen_res) <- c("Obs_mean", "Obs_sd", "Null_mean",
                     "Null_sd", "Z_score", "P_val")
  rownames(gen_res) <- c("Desciptions_cosine", "Desciptions_lv",
                         "Desciptions_jw")
  pdf(file.path(output_dir, paste0(grp, ".pdf")))
  
  # DESCRIPTION DIFFERENCE
  cat('    Testing description difference ....')
  lf_dsts <- plyr::mdply(.data=data.frame(i=1:length(lf_dscrptn)),
                         .fun=function(i){
                           calcStrDst(lf_dscrptn)
                         }, .parallel=TRUE)
  null_dsts <- plyr::mdply(.data=data.frame(i=1:length(lf_dscrptn)),
                         .fun=function(i){
                           tmp <- unlist(null_dscrptn[[i]])
                           calcStrDst(tmp)
                         }, .parallel=TRUE)
  # cosine
  obs_mean <- mean(lf_dsts[,'cosine_median'])
  obs_sd <- sd(lf_dsts[,'cosine_median'])
  null_mean <- mean(null_dsts[, 'cosine_median'])
  null_sd <- sd(null_dsts[, 'cosine_median'])
  p_val <- sum(null_dsts[, 'cosine_median'] <= obs_mean)/nrow(null_dsts)
  z_score <- (obs_mean - null_mean)/null_sd
  hist(null_dsts[, 'cosine_median'],
       main=paste0('P = ', signif(p_val, 3)),
       xlab="Cosine distances of description")
  abline(v=obs_mean, col='red')
  gen_res[1, ] <- c(obs_mean, obs_sd, null_mean,
                null_sd, z_score, p_val)
  # levenshtein
  obs_mean <- mean(lf_dsts[,'lv_median'])
  obs_sd <- sd(lf_dsts[,'lv_median'])
  null_mean <- mean(null_dsts[, 'lv_median'])
  null_sd <- sd(null_dsts[, 'lv_median'])
  p_val <- sum(null_dsts[, 'lv_median'] <= obs_mean)/nrow(null_dsts)
  z_score <- (obs_mean - null_mean)/null_sd
  hist(null_dsts[, 'lv_median'],
       main=paste0('P = ', signif(p_val, 3)),
       xlab="Levenshtein distances of description")
  abline(v=obs_mean, col='red')
  gen_res[2, ] <- c(obs_mean, obs_sd, null_mean,
                null_sd, z_score, p_val)
  # jaro-winkler
  obs_mean <- mean(lf_dsts[,'jw_median'])
  obs_sd <- sd(lf_dsts[,'jw_median'])
  null_mean <- mean(null_dsts[, 'jw_median'])
  null_sd <- sd(null_dsts[, 'jw_median'])
  p_val <- sum(null_dsts[, 'jw_median'] <= obs_mean)/nrow(null_dsts)
  z_score <- (obs_mean - null_mean)/null_sd
  hist(null_dsts[, 'jw_median'],
       main=paste0('P = ', signif(p_val, 3)),
       xlab="Jaro Winkler distances of description")
  abline(v=obs_mean, col='red')
  gen_res[3, ] <- c(obs_mean, obs_sd, null_mean,
                null_sd, z_score, p_val)
  cat("Done.\n")
  dev.off()
  
  # WORD FREQUENCIES
  cat("    Testing for significant word frequencies ....")
  wts <- txts <- NULL
  for(i in 1:length(lf_dscrptn)) {
    tmp_txts <- as.character(unlist(lf_dscrptn[[i]]))
    n <- length(tmp_txts)
    wts <- c(wts, rep(1/n, n))
    txts <- c(txts, tmp_txts)
  }
  obs_frq <- getWrdFrq(txts, wts)
  null_frqs <- vector("list", length=length(null_dscrptn))
  for(i in 1:length(null_dscrptn)) {
    txts <- as.character(unlist(null_dscrptn[[i]]))
    null_frqs[[i]] <- getWrdFrq(txts)
  }
  # how many more times does a term appear in observed?
  frq_res <- data.frame(wrd=NA, obs_frq=NA, exp_frq=NA,
                        p_val=NA, z_score=NA)
  for(i in 1:length(obs_frq)) {
    wrd <- names(obs_frq)[i]
    nd <- unlist(lapply(null_frqs, function(x) {
      if(wrd %in% names(x)) {
        res <- x[[wrd]]
      } else {
        res <- 0
      }
      res
    }))
    frq_res[i, 'wrd'] <- wrd
    frq_res[i, 'obs_frq'] <- obs_frq[[wrd]]
    frq_res[i, 'exp_frq'] <- mean(nd)
    frq_res[i, 'z_score'] <- (obs_frq[[wrd]] - mean(nd))/sd(nd)
    frq_res[i, 'p_val'] <- sum(nd >= obs_frq[[wrd]])/length(nd)
  }
  # word counts
  null_wc <- unlist(lapply(null_frqs, function(x) sum(x)))
  obs_wc <- sum(obs_frq)
  z_score <- (obs_wc - mean(null_wc))/sd(null_wc)
  p_val <- sum(null_wc >= obs_wc)/length(null_wc)
  wc_res <- data.frame(obs=obs_wc, mean_null=mean(null_wc),
                       sd_null=sd(null_wc), z_score, p_val)
  # get word in description table
  wrd_clade <- vector("list", length=nrow(frq_res))
  wrd_res <- vector("list", length=2)
  names(wrd_res) <- c('prnt', 'kids')
  for(wrd in frq_res[['wrd']]) {
    wrd_clade[[wrd]] <- wrd_res
    for(i in 1:length(lf_dscrptn)) {
      prnt <- names(lf_dscrptn)[[i]]
      kids <- names(lf_dscrptn[[i]])
      for(j in 1:length(lf_dscrptn[[i]])) {
        txt <- as.character(lf_dscrptn[[i]][[j]])
        kid <- kids[[j]]
        if(grepl(wrd, txt)) {
          wrd_clade[[wrd]][['prnt']] <-
            c(wrd_clade[[wrd]][['prnt']], prnt)
          wrd_clade[[wrd]][['kids']] <-
            c(wrd_clade[[wrd]][['kids']], kid)
        }
      }
    }
  }
  cat("Done.\n")
  
  # OUTPUT
  save(gen_res, frq_res, wc_res, wrd_clade,
       file=file.path(output_dir, iucn_file))
}

# END
cat(paste0('\nStage `iucn permutation` finished at [', Sys.time(), ']\n'))