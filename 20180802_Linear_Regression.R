rm(list=ls())
setwd(dir = choose.dir())
DF1 <- read.csv(file.choose(), header = T)
DF2 <- aggregate.data.frame(x=DF1$Area, by=list(DF1$Time, DF1$Protein.Name), FUN = sum)
samples <- unique(DF2$Group.2)
n_samples <- as.integer(length(unique(DF2$Group.2)))
DF3 <- unstack(DF2, form = x ~ Group.2)
DF3$time <- as.integer(unique(DF2$Group.1))  
n <- 1
while (n <= n_samples) {
   Mod1 <- lm(log2(DF3[, n]) ~ DF3$time)
   require(broom)
   tidy_Mod1 <- tidy(Mod1)
   slope <- tidy_Mod1[2, 2]
   intercept <- tidy_Mod1[1, 2]
   Half_Time <- (-0.693/slope)
   Half_Time <- as.numeric(Half_Time)
   pdf(paste(samples[n],".pdf",sep="")) 
   plot(DF3$time,log2(DF3[, n]), xlab = "Time / Minute", ylab = "Log (concentartion)", main = c("Half life time (in minutes) for", as.character(samples[n]), "is", round(Half_Time, 0)))
   abline(lm(log2(DF3[, n]) ~ DF3$time), col = "Blue")
   dev.off() 
   pdf(paste(samples[n],"percentage.pdf",sep="_"))
   require(gridExtra)
   signal <- as.data.frame(log2(DF3[, n]))
   i <- length(unique(DF3$time))
   intercept1 <- rep(intercept, times = i)
   result <- round(2^(as.numeric(signal$`log2(DF3[, n])`)-as.numeric(intercept1))*100, 3)
   percentage <- data.frame("Time / minutes" = DF3$time, "Percentage" = result)
   grid.table(percentage)
   dev.off() 
   n <- n + 1
}