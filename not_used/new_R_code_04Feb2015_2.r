# LOAD PACKAGES
library(mgcv)
library(plyr)


# SET WORKING DIRECTORY AND LOAD DATA
# Data file not manipulated prior to import
setwd("/home/eebrandt/projects/temp_trials/male_only/data/")
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
 
# PRELIMINARY PAIRWISE PLOTS
# Just for review.
pairs(dat[, 6:ncol(dat)])

# CLEAN UP DATA AND PLOT HISTOGRAMS
dat$treatment<-factor(dat$treatment)
dat$data.na <- apply(dat,1,function(x) sum(!is.na(x[9:18])))
dat <- dat[which(!dat$data.na < max(dat$data.na)),] # omit NA rows

par(mfrow=c(2,5))
hist(dat$scrape, col="grey50")
hist(dat$srms, col="grey50")
hist(dat$thump, col="grey50")
hist(dat$trms, col="grey50")
hist(dat$buzz, col="grey50")
hist(dat$brms, col="grey50")
hist(dat$srates, col="grey50")
hist(dat$sfreq, col="grey50")
hist(dat$tfreq, col="grey50")
hist(dat$bfreq, col="grey50")

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

# plot residuals to check models
par(mfrow=c(2,5))
lapply(mod.list, function(x) qqnorm(resid(x$gam)))

# In general, residual plots look alright, with a couple of exceptions (scrape, bfreq, etc.). These can be rechecked once the outliers that you mentioned on 12-Feb-2015 are dealt with. 

# PLOT ALL RELATIONSHIPS
# create prediction dataframe
# Note that the raw data points are not yet plotted here.
newdat <- data.frame(temp=seq(min(dat$temp), max(dat$temp), 0.1))

# predict values (remember that link function for Gamma family
# has been modified to the log-link to ease prediction.
par(mfrow=c(2,5))



for (i in seq_along(mod.list)) {
 mod.temp <- mod.list[[i]]
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
 
 # plot

 raw.response <- dat[, title.plot]
 plot(newdat$temp, predfit, lwd=2, xlim=xlim.val, ylim=ylim.val, type="l", main="", ann=F, col="white")
 polygon(x=c(newdat$temp, rev(newdat$temp)), y=c(lower95, rev(upper95)), col="grey80", border=NA)
 par(new=T)
 plot(newdat$temp, predfit, lwd=2, xlim=xlim.val, ylim=ylim.val, type="l", main=title.plot, ylab=title.plot, xlab="temperature")
 par(new=T)
 plot(dat$temp, raw.response, xlim=xlim.val, ylim=ylim.val, type="p", main=title.plot, ylab=title.plot, xlab="temperature", pch=16, col="grey50")
 
}
 
 