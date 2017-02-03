library(ggplot2)

getShrdWrds <- function(types) {
  wrds <- gsub('\\/', ' ', types)
  wrds <- unlist(strsplit(wrds, '\\s'))
  wrds <- gsub('[^a-zA-Z]', '', wrds)
  wrds <- tolower(wrds)
  wrds <- wrds[sapply(wrds, nchar) > 3]
  special_types <- table(wrds)
  names(special_types)[special_types > 1]
}

testing <- function(cds_of_interest, indx) {
  # loop through all the clades and test the proportion of descendents
  # present in the habitats of interest
  # cds_of_interest (named vector of codes or list of vectors of codes)
  p_data <- data.frame(pa=NA, ht=NA)
  p_data[[indx]] <- NA
  cd_tests <- vector("list", length=length(cds_of_interest))
  names(cd_tests) <- names(cds_of_interest)
  for(i in 1:length(cds_of_interest)) {
    vals <- as.numeric(calcCdProp(cds_of_interest[[i]]))
    vals <- as.numeric(vals >= .5)
    if(sum(vals) > 20) {
      tmp_p_data <- data.frame(pa=vals, ht=names(cd_tests)[i])
      tmp_p_data[[indx]] <- epi[[indx]]
      p_data <- rbind(p_data, tmp_p_data)
      test <- t.test(epi[[indx]][vals == 1],
                     epi[[indx]][vals != 1])
      means <- tapply(epi[[indx]], factor(vals), mean, na.rm=TRUE)
      diff <- means[1] - means[2]
      cd_tests[[i]] <- list('p'=test[['p.value']], 'means'=means,
                            'diff'=diff, 'n'=sum(vals))
    }
  }
  p_data <- p_data[-1, ]
  cd_tests <- cd_tests[!sapply(cd_tests, is.null)]
  ns <- sapply(cd_tests, function(x) x[['n']])
  diffs <- sapply(cd_tests, function(x) x[['diff']])
  p_vals <- sapply(cd_tests, function(x) x[['p']])
  ordr <- order(diffs, decreasing=TRUE)
  diffs <- diffs[ordr]
  p_vals <- p_vals[ordr]
  cds <- names(cd_tests)[ordr]
  test_res <- data.frame(cds, diffs, p_vals,
                         stringsAsFactors=FALSE)
  res <- list('test_res'=test_res[p_vals < 0.05, ],
              'p_data'=p_data)
  res
}

ggBinomial <- function(data) {
  #http://docs.ggplot2.org/current/geom_smooth.html
  binomial_smooth <- function(...) {
    geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  }
  # To fit a logistic regression, you need to coerce the values to
  # a numeric vector lying between 0 and 1.
  # add quotes to htypes
  data$type <- paste0("'", data$type, "'")
  p <- ggplot(data, aes(x=pepi, y=scr, colour=type, group=type)) +
    binomial_smooth(aes(fill=type)) +
    ylab("Prop. in habitat") + xlab("pEPI") +
    scale_fill_discrete(name="Habitat") +
    scale_colour_discrete(name="Habitat") +
    theme_bw()
  p
}

calcHbbtProp <- function(hbbt_trm) {
  # count the proportion of descendent species with one of their suitable habbitats
  # containing hbbt_trm
  .test <- function(x) {
    any(grepl(ht, tolower(x)))
  }
  .count <- function(x) {
    sum(sapply(x, .test))/length(x)
  }
  sapply(hbbts, function(x) .count(x))
}

calcCdProp <- function(cd) {
  # count the proportion of descendent species with one of their suitable habbitats
  # containing habbitat code(s)
  .test <- function(x) {
    any(cd %in% x)
  }
  .count <- function(x) {
    sum(sapply(x, .test))/length(x)
  }
  sapply(cds, function(x) .count(x))
}