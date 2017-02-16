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

hbbtPA <- function(cds_of_interest) {
  # loop through all the clades and get pa for habitat
  # cds_of_interest (named vector of codes or list of vectors of codes)
  res <- matrix(NA, nrow=nrow(epi), ncol=length(cds_of_interest))
  colnames(res) <- names(cds_of_interest)
  for(i in 1:length(cds_of_interest)) {
    vals <- as.numeric(calcCdProp(cds_of_interest[[i]]))
    vals <- as.numeric(vals >= .5)
    res[ ,i] <- vals
  }
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