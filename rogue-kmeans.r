#
# Gow, Baumgarten, Cairns, Colton & Miller (2012)
# "Unsupervised Modelling of Player Style with LDA"
# Submitted to IEEE Transactions on Computational Intelligence & AI in Games.
# 
# Supplementry R script for Rogue Trooper data analysis
#
# rogue-kmeans.r
# Apply k-means clustering to mean combats in LDA space
#
# Author: Jeremy Gow
# Edited: 28 March 2012
#

kcluster <- function(data, n, k) {
  kmeans(data[1:n], centers=k, nstart=4)
}

# Compute mean within sum-of-squares (WSS) over 100 kmeans solutions,
# where k varies from 2 to 15
cluster.wss.ld <- function(data, n) {
  runs <- 100
  wss <- (nrow(data[1:n])-1)*sum(apply(data[1:n],2,var))
  for (i in 2:15) {
    tmp <- 0
    for (test in 1:runs) {
      tmp <- tmp + sum(kcluster(data, n, i)$withinss)
    }
    wss[i] <- tmp / runs
  }
  return(wss)
}

##############################################################
#
# Determining appropriate number of kmeans clusters
#

# Plot WSS / number of clusters for LD1-LD2
png(paste(images, "num-clusters2.png", sep=""), res=72)
plot(1:15, cluster.wss.ld(player.means, 2), type="b", xlab="Number of clusters (k)",
     ylab="Within cluster sum of squares (WSS)", mar=c(5.1, 4.1, 2, 2.1))
dev.off()

# Plot WSS / number of clusters for 2, 3 and 4 LDA dimensions
png(paste(images, "num-clusters2-4.png", sep=""), res=72)
plot(1:15, cluster.wss.ld(player.means, 2), type="b", xlab="Number of clusters (k)",
     ylab="Within cluster sum of squares", ylim=c(0, 100), mar=c(5.1, 4.1, 2, 2.1))
points(1:15, cluster.wss.ld(player.means, 3), type="b", pch=2)
points(1:15, cluster.wss.ld(player.means, 4), type="b", pch=3)
legend(12, 100, legend=c("LD1-2", "LD1-3", "LD1-4"),pch=c(1,2,3))
dev.off()


##############################################################
# 
# Exploring alternative kmeans solutions
#

# Run kmeans multiple times, and record the frequency of each
# solution, identified by the total WSS.
sample.kcluster <- function(data, n, k, runs=1000) {
  solns <- NULL
  for (i in 1:runs) {
    # Find a kmeans solution, compute WSS
    wss <- kcluster(data, n, k)$tot.withinss

    if (is.null(solns)) {
      # This is the first solution computed
      solns <- data.frame(wss=wss, count=1)
    } else {
      if (any(solns$wss == wss)) {
        # We have already encountered this solution
        row <- which(solns$wss == wss)
        solns[row, "count"] <- solns[row, "count"] + 1
      } else {
        # This is the first time we've seen this solution
        solns <- rbind(solns, data.frame(wss=wss, count=1))
      }
    } 
  }
  solns$perc <- round(100 * solns$count / runs, 1)
  solns <- solns[order(solns$wss),]
  return (solns)
}

# Try kmeans until a solution with WSS <= max.wss is found
kcluster.bound <- function(data, n, k, wss.bound, runs=1000) {
  soln <- NULL
  for (i in 1:runs) {
    res <- kcluster(data, n, k)
    if (res$tot.withinss <= wss.bound) {
      soln <- res
      break
    }
  }
  return (soln)
}

# Find the minimum WSS solution
kcluster.min <- function(data, n, k) {
  min.wss <- min(sample.kcluster(data, n, k)$wss)
  return(kcluster.bound(data, n, k, min.wss))
}

# For a partitioning of items into N groups, a list of N group labels, and
# a single example item for each label, reorder the labels so that they
# correctly label the groups 1 to N.
reorder.labels <- function(groups, labels, examples) {
  if (length(labels) != length(examples)) {
    stop("labels and examples lists are not of equal length")
  }
  
  relabels <- rep(NA, length(labels))

  for (i in 1:length(labels)) {
    # example[i] is in the jth group
    j <- groups[examples[i]]
    # Hence label[i] should be moved to the jth position
    # First check that jth position hasn't already been assigned
    if (is.na(relabels[j])){
      relabels[j] <- labels[i]
    } else {
      # Error: try to assign two labels to the same group
      stop(paste("Group", j, "has multiple labels:", relabels[j], "and", labels[j]))
    }
  }
  return(relabels)
}

##############################################################
#
# For LD1-LD2 space, find 4 cluster solution with minimum WSS
#
# It's possible (though unlikely) that we won't find the required clu4,
# in which case the script will end with an error.
#
clu4.target <- c("Hyperactive", "Naive", "Normal", "Timid")
clu4.labels <- c("", "", "", "")
while (any(clu4.labels != clu4.target)) {
  clu4 <- kcluster.min(player.means, 2, 4)
  clu4.labels <- reorder.labels(clu4$cluster, clu4.target, c(15, 29, 24, 30))
}

plot.kcluster <- function(means, clu, leg=NULL) {
  tmp <- data.frame(means, clu$cluster)
  plot(LD2 ~ LD1, data=tmp, pch=clu$cluster)
  if (!is.null(leg)) {
    legend(0.85, 1.8, legend=leg, pch=1:length(leg))
  }
}

png(paste(images, "kmeans-ld2-k4.png", sep=""), res=72)
plot.kcluster(player.means, clu4, clu4.labels)
dev.off()

##############################################################
# 
# Group differences for 4 cluster solution
#

players <- read.csv("rogue-players.csv")

often.lev <- c("Every day", "Every week", "Every month", "Few times year", "Once year less")
age.lev <- c("Under 16", "16-25", "26-35", "36-45", "46-55", "56-65", "Over 65")
exp.lev <- c("Less than a year", "1-5 years", "5-10 years", "10-20 years", "More than 20")

players$often <- factor(players$often, levels=often.lev, ordered=T)
levels(players$often) <- c("Daily", "Weekly", "Monthly", "Yearly", "Less")
players$age <- factor(players$age, levels=age.lev, ordered=T)
players$experience <- factor(players$experience, levels=exp.lev, ordered=T)

sessions <- read.csv("rogue-sessions.csv")
players <- merge(players, sessions)
# If player played twice, remove earlier attempt
players <- players[players$log_id != 1 & players$log_id != 3 & players$log_id != 4,]


players.ld <- data.frame(player_id=1:32, LD1=player.means[1], LD2=player.means[2], LD3=player.means[3], groupN=clu4$cluster)
players.ld$group <- factor(players.ld$groupN, labels=clu4.labels)
players <- merge(players, players.ld)

png(paste(images, "freq-ld1.png", sep=""), res=72)
plot(LD1 ~ often, data=players, xlab="Gaming frequency")
dev.off()

plot.for.clusters <- function(var, ylab) {
   png(paste(images, var, "-ld2-k4.png", sep=""), res=72)
   form <- as.formula(paste(var,"~ group"))
   plot(form, data=players, xlab=NULL, ylab=ylab)
   dev.off()
}

plot.for.clusters("deaths", "Player deaths")
plot.for.clusters("ptime", "Seconds played")
plot.for.clusters("grenades", "Grenades")
plot.for.clusters("imm", "Immersion")
