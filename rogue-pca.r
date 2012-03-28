#
# Gow, Baumgarten, Cairns, Colton & Miller (2012)
# "Unsupervised Modelling of Player Style with LDA"
# Submitted to IEEE Transactions on Computational Intelligence & AI in Games.
# 
# Supplementry R script for Rogue Trooper data analysis
#
# rogue-pca.r
# Apply PCA to Rogue combat feature data and compare with LDA
#
# Author: Jeremy Gow
# Edited: 28 March 2012
#
library(FactoMineR)
library(fpc)

pca.res <- PCA(fea3, graph=F, quali.sup=1)
pca.means <- data.frame(pca.res$quali.sup$coord)
pca.means$player_id <- 1:32

png(paste(images, "pca-ind.png", sep=""))
plot(pca.res)
dev.off()

png(paste(images, "pca-var.png", sep=""))
plot(pca.res, choix="var")
dev.off()

png(paste(images, "pca-cent.png", sep=""))
plot(NULL, xlim=c(-2,3), ylim=c(-2,2), xlab="PC1", ylab="PC2")
text(pca.means$Dim.1, pca.means$Dim.2, pca.means$player_id, pos=4)
dev.off()


#
# Extract PCA loadings
#
pca.loadings <- function(d=NULL) {
  load <- data.frame(sweep(pca.res$var$coord,2,sqrt(pca.res$eig[1:ncol(pca.res$var$coord),1]),FUN="/"))
  
  if (is.numeric(d)) {
    return(round(load[d], 2))
  } else {
    return(load)
  }
}

#
# Separation of players vs. separation of combat scenarios
#

# Compute alternative PCA object
pca.res2 <- prcomp(fea3[-1])

# Combats in PCA space
pred.pca <- data.frame(predict(pca.res2))
# Combats in LDA space
pred.lda <- data.frame(predict(lda.res))

# Cluster statistics for each player's combats
pca.stats <-  cluster.stats(dist(pred.pca[1:2]), fea3$player_id)
lda.stats <-  cluster.stats(dist(pred.lda[1:2]), fea3$player_id)

# Ratio of between-cluster to within-cluster for player clusters, for both PCA and LDA
player.bw.pca <- pca.stats$average.between / pca.stats$average.within
player.bw.lda <- lda.stats$average.between / lda.stats$average.within

# Find the combat scenario number for each combat
task.nums <- as.numeric(unlist(lapply(strsplit(rownames(fea3), ".", fixed=T), function(x) {x[2]})))

# Cluster statistics for each combat scenario
pca.stats2 <-  cluster.stats(dist(pred.pca[1:2]), task.nums)
lda.stats2 <-  cluster.stats(dist(pred.lda[1:2]), task.nums)

# Ratio of between-cluster to within-cluster for player clusters, for both PCA and LDA
scenario.bw.pca <- pca.stats2$average.between / pca.stats2$average.within
scenario.bw.lda <- lda.stats2$average.between / lda.stats2$average.within

