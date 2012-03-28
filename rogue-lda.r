#
# Gow, Baumgarten, Cairns, Colton & Miller (2012)
# "Unsupervised Modelling of Player Style with LDA"
# Submitted to IEEE Transactions on Computational Intelligence & AI in Games.
# 
# Supplementry R script for Rogue Trooper data analysis
#
# rogue-lda.r
# Prepare Rogue combat feature data and apply LDA
#
# Author: Jeremy Gow
# Edited: 28 March 2012
#
# NOTE: create a directory 'images' before running this script
#
library(MASS)
setEPS()
images <- "images/"

# Load feature data for 881 Rogue Trooper combats
fea <- read.csv("rogue-combats.csv")

#########################################################
#
# Select subset of feature data for analysis
#
# 550 level 1 combats
fea <- fea[fea$level == 1,] 
# 462 combats survived by PC
fea <- fea[!fea$death,]
# 431 combats where PC survives and we're sure no NPCs do
fea <- fea[fea$npcs == fea$npc.dead,]

# Remove one further combat where we know player abandons combat
# and backtracks through level.
fea <- fea[fea$log_id != 51 | fea$life != 1 | fea$task_id != 9,]

# Cap outlier values (6 values in total)
fea$prefire[fea$prefire > 90] <- 90
fea$npc.postfire[fea$npc.postfire > 60] <- 60
fea$area.rate[fea$area.rate > 30] <- 30
fea$turn.rate[fea$turn.rate > 1.5] <- 1.5
fea$npc.lead[fea$npc.lead > 40] <- 40

lda.vars <- c("prefire", "npc.postfire", "mean.postdam", "npc.lead", "mean.npc.fire", "firing", "sniping",
              "mean.aim", "gren.rate", "cover", "pistol", "mean.final.dist", "move", "dist.rate", "turn.rate",
              "area.rate", "zero.ammo", "mean.ammo", "pre.ammo", "mean.health", "dam.rate")

#########################################################
#
# Compute mean features for each player-NPC group pair
#
player.combat.means <- function(cdata) {
  tmp <- NULL
  for (v in lda.vars) {
    ptm <- aggregate(as.formula(paste(v, "~ player_id + task_id")), data=cdata, mean)
    if (is.null(tmp)) {
      tmp <- ptm
    } else {
      tmp <- merge(tmp, ptm, all=T, sort=F)
    }
  }
  # Add row names
  tmp <- data.frame(tmp, row.names=paste(tmp$player_id, tmp$task_id, sep="."))
  return(tmp)
}

fea2 <- player.combat.means(fea)[-2]
fea2$npc.lead[is.na(fea2$npc.lead)] <- 0
fea2 <- fea2[order(fea2$player_id),]

# Standardised features - subtract mean and divide by standard dev.
fea3 <- data.frame(fea2[1], scale(fea2[-1]))


#################################################
#
# Assess equality of player's covariance matrices
#

# Remove any columns which can be all zeros for a given player
fea.nz <- subset(fea3, select=-c(mean.aim, cover, gren.rate, pistol, zero.ammo, sniping))

# Returns the 15-feature covariance matrix for a given player
player.cov <- function(i) {
  return(cov(fea.nz[fea.nz$player_id == i, -1]))
}

# Log determinants of player covariance matrices
logDets <- NULL 
for (i in 1:32) {
  logDets[i] <- determinant(player.cov(i), logarithm=T)$modulus
}

# Distributed in approx. 3 clumps
png(paste(images, "player-logdets.png", sep=""), res=72)
hist(logDets, n=20, xlab="Log determinant of covariance for players")
dev.off()

######################################################
#
# Perform LDA
#
# Construct variable formula
lda.form <- as.formula(paste("player_id ~", paste(lda.vars, collapse=" + ")))

# Perform LDA with mean feature data
lda.res <- lda(lda.form, data=fea3) 

# Extract player means in LDA space
player.means <- data.frame(predict(lda.res, data.frame(lda.res$means))$x)

######################################################
#
# Plot LDA results
#

xlab <- "LD1"
ylab <- "LD2"

png(paste(images, "player-lda.png", sep=""), res=72)
plot(lda.res, dimen=2, cex=0.8, xlab=xlab, ylab=ylab)
dev.off()

png(paste(images, "player-centroids.png", sep=""), res=72)
plot(LD2 ~ LD1, data=player.means, xlab=xlab, ylab=ylab, pch=" ")
text(LD2 ~ LD1, data=player.means, labels=1:32)
dev.off()


######################################################################
#
# Find LD/feature correlations
#

fea4 <- cbind(fea3, data.frame(predict(lda.res, data.frame(fea3[,-1]))$x))

# Display ordered coefficients for linear discriminant
lda.coeff <- function(ld=1) {
  data.frame(sort(lda.res$scaling[, ld]))
}

lda.coeffs <- function() {
    sca <- lda.res$scaling
    max.ld <- dim(sca)[2]
    ld.names <- dimnames(sca)[[2]]
    coeffs <- data.frame(feature=row.names(sca))
    for (i in 1:max.ld) {
      coeffs[ld.names[i]] <- round(sca[,i], 2)
    }
    return (coeffs)
}


lda.cor <- function() {
  res <- NULL
  for (v in lda.vars) {
    cor1 <- round(cor(fea4[v], fea4$LD1, use="complete.obs"), 2)
    cor2 <- round(cor(fea4[v], fea4$LD2, use="complete.obs"), 2)
    cor3 <- round(cor(fea4[v], fea4$LD3, use="complete.obs"), 2)
    row <- data.frame(feature=v, LD1=cor1, LD2=cor2, LD3=cor3)
    if (is.null(res)) {
      res <- row  
    } else {
      res <- rbind(res, row)
    }
  }
  return (res)
}

# Feature coefficients and correlations for LD1 and LD2
lda.interpret <- function(ld=0, co=0.3, rho=0.5) {
  acoef <- lda.coeffs()[1:4]
  names(acoef) <- c("feature", "LD1.co", "LD2.co", "LD3.co")

  acor <- lda.cor()
  names(acor) <- c("feature", "LD1.rho", "LD2.rho", "LD3.rho")
  
  tmp <- merge(acoef, acor)
  
  if (ld == 1 | ld == 2 | ld == 3) {
    tmp <- tmp[,c(1, ld + 1, ld + 3)]
    tmp <- tmp[abs(tmp[,2]) >= co | abs(tmp[,3]) >= rho,]
    
    tmp.pos <- tmp[tmp[,2] > 0,]
    tmp.pos <- tmp.pos[order(tmp.pos[,3], decreasing=T),]

    tmp.neg <- tmp[tmp[,2] < 0,]
    tmp.neg <- tmp.neg[order(tmp.neg[,3]),]
    
    tmp <- rbind(tmp.pos, tmp.neg)
  }
  
  return (tmp)
}

lda.co <- lda.interpret()


