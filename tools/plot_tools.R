library(ggplot2)

# synonyms

ggBoxplot <- function(score, x="pepi") {
  p_data <- data.frame(score=epi[[score]],
                       epi=epi[[x]])
  p_data <- na.omit(p_data)
  p <- ggplot(p_data, aes(factor(score), epi)) +
    geom_boxplot()
  p + theme_bw(fill="cornflowerblue")
}

ggViolin <- function(score, x="pepi") {
  p_data <- data.frame(score=epi[[score]],
                       epi=epi[[x]])
  p_data <- p_data[p_data$score %in% c(-1, 1), ]
  p_data$score[p_data$score == -1] <- "antonym"
  p_data$score[p_data$score == 1] <- "synonym"
  p_data$score <- factor(p_data$score, levels=c('synonym',
                                                "antonym"))
  p <- ggplot(p_data, aes(score, epi, fill=score)) + geom_violin()
  p <- p + theme_bw() + theme(axis.text.x=element_blank(),
                              legend.position="none")
  p
}

ggComps <- function(p_data, scr, indx, adjust=1) {
  p_data <- data.frame(score=p_data[[scr]], epi=p_data[[indx]])
  p_data <- p_data[p_data$score %in% c(-1, 1), ]
  p_data$score[p_data$score == -1] <- "antonym"
  p_data$score[p_data$score == 1] <- "synonym"
  p_data$score <- factor(p_data$score, levels=c('synonym',
                                                "antonym"))
  p <- ggplot(p_data, aes(epi, type=score, colour=score, fill=score)) +
    geom_density(adjust=adjust, alpha=.75)
  p <- p + theme_bw() + theme(axis.text.x=element_blank(),
                              legend.position="none")
  p
}

ggWrdBars <- function(wrd_counts, pstvs, ngtvs) {
  pstv_counts <- data.frame(wrd=names(wrd_counts[pstvs]), count=wrd_counts[pstvs],
                            type='synonym')
  ngtv_counts <- data.frame(wrd=names(wrd_counts[ngtvs]), count=wrd_counts[ngtvs],
                            type='antonym')
  ordrd_levels <- c(as.character(pstv_counts$wrd[order(pstv_counts$count, decreasing=FALSE)]),
                    as.character(ngtv_counts$wrd[order(ngtv_counts$count, decreasing=TRUE)]))
  p_data <- rbind(pstv_counts, ngtv_counts)
  p_data$wrd <- factor(p_data$wrd, levels=ordrd_levels)
  ggplot(p_data, aes(x=wrd, y=count, fill=type)) +
    coord_flip() + geom_bar(stat="identity") +
    ylab("Freq.") + xlab("") + theme_bw() +
    theme(legend.title=element_blank())
}