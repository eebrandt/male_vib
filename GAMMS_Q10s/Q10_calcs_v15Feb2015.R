# LOAD PACKAGES
library(mgcv)
library(plyr)
library(plotrix)


# SET WORKING DIRECTORY AND LOAD DATA
# Data file not manipulated prior to import


setwd("/home/eebrandt/projects/temp_trials/male_only/src/GAMMS_Q10s/")
#setwd("C:/Users/Patrick/Documents/research_projects/Erin_Brandt_temperature")
dat <- read.csv("temp_vib_data_toPatrick.csv")

# CLEAN DATA
dat$tape.AF8.video <- NULL
dat$comments <- NULL
dat <- dat[which(!dat$complete=="NA"), ]
colnames(dat) <- c(
  "complete",
  "ind",
  "treatment",
  "rank",
  "date",
  "temp",
  "weight",
  "ct.width",
  "scrape",
  "srms",
  "thump",
  "trms",
  "buzz",
  "brms",
  "srates",
  "sfreq",
  "tfreq",
  "bfreq"
)
dat$ind <- factor(dat$ind)

# CLEAN UP DATA AND PLOT HISTOGRAMS
dat$treatment<-factor(dat$treatment)
dat$data.na <- apply(dat,1,function(x) sum(!is.na(x[9:18])))
dat <- dat[which(!dat$data.na < max(dat$data.na)),] # omit NA rows
#dat$scrape <- dat$scrape*1000
#dat$thump <- dat$thump*100
#dat$buzz <- dat$buzz*100



par(mfrow=c(2,5))

# MODEL (GAMM)
# Use Gamma family for errors (as it seems that data are positively
# and bounded on the lower end by 0. 
# Using gamm() in mgcv package, but could use other functions from other packages. 

# define knots for GAM and run models
knots.val <- 15

mod.list <- list() # create model list
mod.list[[1]] <- gamm(scrape~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[2]] <- gamm(srms~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[3]] <- gamm(thump~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[4]] <- gamm(trms~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[5]] <- gamm(buzz~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[6]] <- gamm(brms~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[7]] <- gamm(srates~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[8]] <- gamm(sfreq~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[9]] <- gamm(tfreq~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)
mod.list[[10]] <- gamm(bfreq ~ s(temp, k=knots.val), random=list(ind=~1), family=Gamma(link="log"), data=dat)


# In general, residual plots look alright, with a couple of exceptions (scrape, bfreq, etc.). These can be rechecked once the outliers that you mentioned on 12-Feb-2015 are dealt with. 

# PLOT ALL RELATIONSHIPS
# create prediction dataframe
# Note that the raw data points are not yet plotted here.

# vectors to hold global and spot Q10 values for each variable
globalQ10 <- vector(mode = "numeric")
globalSE <- vector(mode = "numeric")
spotQ10 <- vector(mode = "numeric")


# predict values (remember that link function for Gamma family
# has been modified to the log-link to ease prediction.
output <- list()
par(mfrow=c(2,5))
for (i in seq_along(mod.list)) {
  mod.temp <- mod.list[[i]]
  var.name <- colnames(mod.temp$gam$model)[1]
  
  # prediction data frame
  newdat <- data.frame(temp=seq(
   min(mod.temp$gam$model$temp), 
   max(mod.temp$gam$model$temp), 
   0.05))

  
  # OVERALL MODEL PREDICTION FOR PLOTTING (?)
  pred <- predict(mod.temp$gam, newdata=newdat, type="link", se.fit=T)$fit
  se <- predict(mod.temp$gam, newdata=newdat, type="link", se.fit=T)$se.fit
  predfit <- exp(pred) # 50% percentile of estimate
  lower95 <- exp(pred-1.96*se)
  upper95 <- exp(pred+1.96*se)
  
  # set plot limits
  title.plot <- colnames(mod.temp$gam$model)[1]
  raw.response <- dat[, title.plot]
  response <- mod.temp$gam$model[,1]
  xlim.val <- c(20, 50)
  ylim.val <- c(min(raw.response), max(raw.response))  # add 5% to scale
  
  
  
  
  # Q10 CALCULATION FUNCTION
  q10_function <- function () {
  # Draw two random temperatures from "newdat" data.frame
  # Note that Q10 equation does not require 10degC intervals
  min.temp <- max(newdat$temp)-10
  samp.dat <- subset(newdat, newdat$temp<=min.temp)
  low.temp <- sample(samp.dat$temp, 1)
  two.temps <- data.frame(temp=c(low.temp, low.temp+10))
  
  # predict from model
  two.temps$pred <- predict(mod.temp$gam, newdata=two.temps, type="link", se.fit=T)$fit
  two.temps$se <- predict(mod.temp$gam, newdata=two.temps, type="link", se.fit=T)$se.fit
  
 # draw from distribution (SE approximates SD for normal dist in GLM)
  # keep on link scale
  rate1 <- exp(rnorm(1, two.temps$pred[1], two.temps$se[1]))
  rate2 <- exp(rnorm(1, two.temps$pred[2], two.temps$se[2]))
  q10 <- (rate2/rate1)^(10/(two.temps$temp[2]-two.temps$temp[1]))
  q10calc <-  c(q10calc, q10)
  }
  
  # makes a vector to hold all the values for the globalvalues
  q10calc <- vector(mode = "numeric") 
  
  # 1000 random Q10 calculations
  x <- replicate(1000, q10_function())
  #x <- subset(x, x<20) # eliminate some outlier
  a <- as.numeric(quantile(x, prob=c(0.025, 0.5, 0.975))) # calculate percentiles
  hi.se <- (a[3] - a[2])/1.96 # upper SE
  lo.se <- (a[2] - a[1])/1.96 # upper SE
  
  # summarize into data frame
  output[[i]] <- data.frame(variable=var.name, lo95=a[1], q10.val=a[2], hi95=a[3], lo.se=lo.se, hi.se=hi.se)

      
  # plot
  #raw.response <- dat[, title.plot]
  #plot(newdat$temp, predfit, lwd=2, xlim=xlim.val, ylim=ylim.val, type="l", main="", ann=F, col="white")
  #polygon(x=c(newdat$temp, rev(newdat$temp)), y=c(lower95, rev(upper95)), col="grey80", border=NA)
  #par(new=T)
  #plot(newdat$temp, predfit, lwd=2, xlim=xlim.val, ylim=ylim.val, type="l", main=title.plot, ylab=title.plot, xlab="temperature")
  #par(new=T)
  #plot(dat$temp, raw.response, xlim=xlim.val, ylim=ylim.val, type="p", main=title.plot, ylab=title.plot, xlab="temperature", pch=16, col="grey50")
}

par(1,1)

# SAVE OUTPUT
output <- ldply(output)
write.csv(output, "q10_calcs.csv", row.names=F)

# PLOTTING
# very basic just to get an idea (95% CI plotted)
plotCI(
 x=1:nrow(output), 
 y=output$q10.val,
 uiw=output$hi95-output$q10.val, 
 liw=output$q10.val-output$lo95, 
 ann=F
 )

plot()

#===============================
# Erin's code for plotting below
#===============================


#makes our Q10 data frame containing global q10, spot q10, and SE for global
#Q10_total <- data.frame(globalQ10,globalSE, spotQ10)

# calculates mean +/ SE for the global Q10
#SEplus <- globalQ10 + globalSE
#SEminus <- globalQ10 - globalSE

# plots both global (+/- SE) and spot Q10 values
#par(mfrow=c(1,2))
#plot(SEplus, type = "p", col = "red", pch = 16, cex = .5, axes = FALSE, main = "Global Q10 Values", ylab = "Global Q10 +/- SE")
#axis(1, at = 1:10, labels = names(globalQ10), cex.axis = .43)
#axis(side = 2, ylim = c(.93, 1.06), ylab = "Q10 +/- SE")
#lines(SEminus, type = "p", col = "red", pch = 16, cex = .5)
#lines(globalQ10, type = "p", bg = "black", pch = 16, cex = .5)
#box()

#plot(spotQ10, type = "p", pch = 16, col = "black", cex = .5, main = "Spot Q10 Values", axes = FALSE, ylab = "Spot Q10", xlab = "")
#axis(1, at = 1:10, labels = names(globalQ10), cex.axis = .43)
#axis(side = 2)
#box()

output[1, 2] = 1/output[1,2]
output[1, 3] = 1/output[1,3]
output[1, 4] = 1/output[1,4]
output[1, 5] = 1/output[1,5]
output[1, 6] = 1/output[1,6]

output[3, 2] = 1/output[3,2]
output[3, 3] = 1/output[3,3]
output[3, 4] = 1/output[3,4]
output[3, 5] = 1/output[3,5]
output[3, 6] = 1/output[3,6]

output[5, 2] = 1/output[5,2]
output[5, 3] = 1/output[5,3]
output[5, 4] = 1/output[5,4]
output[5, 5] = 1/output[5,5]
output[5, 6] = 1/output[5,6]

plot(output$q10.val~output$variable)
lines()

#output <- ldply(output)
write.csv(output, "q10_calcs.csv", row.names=F)