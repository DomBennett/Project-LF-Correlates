matchMatrix <- function(pttrns) {
  # Match patterns to words
  .mtch <- function(i) {
    tmp <- rep(NA, length(pttrns))
    for(j in 1:length(pttrns)) {
      tmp[j] <- any(grepl(pttrns[j], wrds[[i]]))
    }
    tmp
  }
  res <- plyr::mdply(wwrds, .fun=.mtch)
  res <- res[ ,-1]
  colnames(res) <- pttrns
  rownames(res) <- wwrds
  res <- res[rowSums(res) > 1,]
  res
}

senseScores <- function(mtch_mtrx, pstvs, ngtvs) {
  # Calculate the sense score
  # A measure of whether descriptions match a given meaning
  # based on synonyms (pstvs) and antonyms (ngtvs)
  # scores are 0, -1 or +1
  # score 1 -- at least 1 pstv or ngtv match only
  scr1 <- (rowSums(mtch_mtrx[ ,pstvs]) > 0) - (rowSums(mtch_mtrx[ ,ngtvs]) > 0)
  # score 2 -- most of pstv or ngtv
  scr2 <- rowSums(mtch_mtrx[ ,pstvs]) - rowSums(mtch_mtrx[ ,ngtvs])
  scr2 <- scr2/abs(scr2)
  scr2[is.na(scr2)] <- 0
  # return
  data.frame(scr1, scr2)
}

getExampleTextFrmI <- function(epi, i) {
  nms <- getKidNms(epi[i,'txid'])
  res <- NULL
  for(nm in nms) {
    nrrtv <- getIUCNNrrtv(nm, token)
    if(class(nrrtv) == "list" && length(nrrtv[['result']]) > 0 &&
       !is.null(nrrtv[['result']][[1]][['habitat']])) {
      nrrtv <- nrrtv[['result']][[1]][['habitat']]
      nrrtv <- gsub("<.*?>", "", nrrtv)  # remove html tags
      res <- c(res, nrrtv)
    }
  }
  res
}

getExampleTextFrmWrd <- function(epi, pttrn) {
  # Get example text from a word a pattern
  bool <- sapply(wrds, function(x) any(grepl(pttrn, x)))
  i <- sample(which(bool), 1)
  getExampleTextFrmI(epi, i)
}

writeOutExampleTexts <- function(epi, mtch_mtrx, scores, fname) {
  # write example texts to file along with pepi, score and found patterns
  # ignore any clade with more than 2 species
  res <- ''
  is <- as.numeric(rownames(mtch_mtrx))
  for(i in 1:nrow(mtch_mtrx)) {
    epi_i <- as.numeric(rownames(mtch_mtrx))[i]
    n <- epi[epi_i, 'n']
    if(n > 2) {
      next
    }
    header <- paste0(epi[epi_i, 'scinm'], '\n',
                     paste0(names(scores[i, ]), collapse='|'), '\n',
                     paste0(scores[i, ], collapse='|'), '\n')
    mtchs <- colnames(mtch_mtrx)[as.logical(mtch_mtrx[i, ])]
    mtchs <- paste0('Matches: ', paste0(mtchs, collapse=', '), '\n')
    txt <- getExampleTextFrmI(epi, epi_i)
    res <- paste0(res, '\n\n-----------------------', header, mtchs, txt)
  }
  write.table(x=res, file=fname, quote=FALSE, row.names=FALSE,
              col.names=FALSE)
}

loopTests <- function(scores) {
  # test and plot
  test_1 <- wilcox.test(scores$pepi[scores$scr1 == 1],
                        scores$pepi[scores$scr1 == -1])
  if(test_1$p.value < 0.05) {
    cat('-- scr1~pepi is significant:\n')
    print(test_1)
    print(tapply(scores$pepi, factor(scores$scr1), mean, na.rm=TRUE))
  }
  test_2 <- wilcox.test(scores$pepi[scores$scr2 == 1],
                        scores$pepi[scores$scr2 == -1])
  if(test_2$p.value < 0.05) {
    cat('-- scr2~pepi is significant:\n')
    print(test_1)
    print(tapply(scores$pepi, factor(scores$scr2), mean, na.rm=TRUE))
  }
  if(sum(!is.na(scores$epi[scores$scr1 == 1])) > 20 &
     sum(!is.na(scores$epi[scores$scr1 == -1])) > 20) {
    test_3 <- wilcox.test(scores$epi[scores$scr1 == 1],
                          scores$epi[scores$scr1 == -1])
    if(test_3$p.value < 0.05) {
      cat('-- scr1~epi is significant:\n')
      print(test_3)
      print(tapply(scores$epi, factor(scores$scr1), mean, na.rm=TRUE))
    }
    test_4 <- wilcox.test(scores$pepi[scores$scr2 == 1],
                          scores$pepi[scores$scr2 == -1])
    if(test_4$p.value < 0.05) {
      cat('-- scr2~epi is significant:\n')
      print(test_1)
      print(tapply(scores$epi, factor(scores$scr2), mean, na.rm=TRUE))
    }
  }
}