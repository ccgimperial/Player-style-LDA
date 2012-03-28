#
# Gow, Baumgarten, Cairns, Colton & Miller (2012)
# "Unsupervised Modelling of Player Style with LDA"
# Submitted to IEEE Transactions on Computational Intelligence & AI in Games.
# 
# Supplement: R script for Snakeotron data analysis
# Author: Robin Baumgarten
# Edited: 28 March 2012
#
dataExt <- read.table(file="snakotron-lda.r", sep=",",header = T)
d <- scale(dataExt[,1:18])
pc1 <- prcomp(d, scale. = T)
colvec <- dataExt$class
ld2 <- MASS::lda(d, grouping = dataExt$class)
x2 <- ld2$scaling
plot(ld2, dimen=2, pch = ".", col = colvec, cex=0.8)
write.table(ld2$scaling, file = "latest_scaling.csv", sep=",")
