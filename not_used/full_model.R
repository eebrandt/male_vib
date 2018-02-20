#GAMwith various vibratory characteristics
library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("mgcv", lib.loc="/usr/lib/R/library")
library("gvlma", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

setwd("~/projects/temp_trials/male_only/data")
details <- file.info(list.files(pattern="temp_vibration_data*"))
details <- details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
loadfile <- tail(files,1)
overall <-read.csv(loadfile)
complete <-subset(overall, complete == TRUE)
setwd("~/projects/temp_trials/male_only/analysis/")
foverall <- overall[ which(overall$sfreq_avg < 150 ), ]

temperature <- overall$temperature
individual <- overall$individual

#Bog-standard linear model
M0_scrape <- lm(scrape_avg ~  temperature, data = overall)
summary(M0_scrape)
M00_scrape <-gls(scrape_avg ~ temperature, data = overall)

# Check our assumptions 
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(M0_scrape, add.smooth = FALSE, which = 1)
E <- resid(M0_scrape)
hist(E, xlab = "Residuals", main = "")
plot(overall$scrape_avg, E, xlab = "Scrape Duration",
     ylab = "Residuals")
par(op)

# Eeks!  Looks like we need a serious variance structure here.  Let's try a few.

# Fixed?
vf1Fixed <- varFixed(~scrape_avg)
Mgls1_scrape <- gls(scrape_avg ~ temperature, weights = vf1Fixed, data = overall)
anova(M00_scrape, Mgls1_scrape)

# Yes, a little better.  VarIdent?
vf2 <-varIdent(form = ~1| scrape_avg)
Mgls2_scrape <- gls(scrape_avg ~ temperature, weights = vf2, data = overall)
AIC(M00_scrape, Mgls1_scrape, Mgls2_scrape)

# Nope, worse.  Power?
vf3 <- varPower(form=~scrape_avg)
Mgls3_scrape <- gls(scrape_avg ~ temperature, weights = vf3, data = overall)
AIC(M00_scrape, Mgls1_scrape, Mgls2_scrape, Mgls3_scrape)

# Power is better. One more try with Varexp.
vf4 <- varExp(form=~scrape_avg)
Mgls4_scrape <- gls(scrape_avg ~ temperature, weights = vf4, data = overall)
AIC(M00_scrape, Mgls1_scrape, Mgls2_scrape, Mgls3_scrape, Mgls4_scrape)


# ..varconst. power now
vf5 <- varConstPower(form=~scrape_avg)
Mgls5_scrape <- gls(scrape_avg ~ temperature, weights = vf5, data = overall)
AIC(M00_scrape, Mgls1_scrape, Mgls2_scrape, Mgls3_scrape, Mgls4_scrape, Mgls5_scrape)

#Varconstpower is best. Let's look at those plots again
E1 <- resid(Mgls5_scrape)
coplot(E1~scrape_avg|temperature,
  ylab = "Ordinary residuals", data = overall)
E2<-resid(Mgls5_scrape)
coplot(E2~scrape_avg|temperature, data = overall, ylab = "Normalized residuals")

Mglm1_scrape <- lme(scrape_avg ~ temperature, weights = vf5, data=overall, random = ~1|individual)

#...let's try GAM now
GAM1_scrape <- gam(overall$scrape_avg~s(temperature, fx = FALSE, k=-1, bs = "cr"), bs="re")
GAM2_scrape <- gam(overall$scrape_avg~s(temperature, fx = FALSE, k=-1, bs = "cr")+ s(overall$individual, bs="re"))

# wow, GAM's looking really good.
AIC(Mgls5_scrape, GAM1_scrape, GAM2_scrape)

# let's plot GAM
E1 <- resid(GAM2_scrape)
coplot(E1~scrape_avg|temperature,
       ylab = "Ordinary residuals", data = overall)
E2<-resid(GAM2_scrape)
coplot(E2~scrape_avg|temperature, data = overall, ylab = "Normalized residuals")

# How about GAMM?
GAMM1_scrape <- gamm(scrape_avg ~ s(temperature, fx = FALSE, k = -1, bs = "cr"), random=list(individual=~1),  weights = vf5, data = overall)
anova(GAM2_scrape, GAMM1_scrape)
summary(GAMM1_scrape)

E1 <- resid(GAMM1_scrape)
coplot(E1~scrape_avg|temperature,
       ylab = "Ordinary residuals", data = overall)
E2<-resid(GAMM1_scrape)
coplot(E2~scrape_avg|temperature, data = overall, ylab = "Normalized residuals")

# Move to thump_avg

#Bog-standard linear model
M0_thump <- lm(thump_avg ~  temperature, data = overall)
# make gls model so we can compare it to others
M00_thump <-gls(thump_avg ~ temperature, data = overall)

# Check our assumptions 
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(M0_thump, add.smooth = FALSE, which = 1)
E <- resid(M0_thump)
hist(E, xlab = "Residuals", main = "")
plot(overall$thump_avg, E, xlab = "thump Duration",
     ylab = "Residuals")
par(op)

# looks a little skewed, although not as bad as scrape.  Let's try giving it a variance structure
# Fixed?
vf1_thump <- varFixed(~thump_avg)
Mgls1_thump <- gls(thump_avg ~ temperature, weights = vf1Fixed, data = overall)
anova(M00_thump, Mgls1_thump)


# Better. Varident? This gives an error, so let's try a different one.
vf2_thump <-varIdent(form = ~1| thump_avg)
Mgls2_thump <- gls(thump_avg ~ temperature, weights = vf2_thump, data = overall)
AIC(M00_thump, Mgls1_thump, Mgls2_thump)

# Power?  A little better.
vf3_thump <- varPower(form=~thump_avg)
Mgls3_thump <- gls(thump_avg ~ temperature, weights = vf3_thump, data = overall)
AIC(M00_thump, Mgls1_thump, Mgls3_thump)

#Varexp?
vf4_thump <- varExp(form=~thump_avg)
Mgls4_thump <- gls(thump_avg ~ temperature, weights = vf4_thump, data = overall)
AIC(M00_thump, Mgls1_thump, Mgls3_thump, Mgls4_thump)

#Better
vf5_thump <- varConstPower(form=~thump_avg)
Mgls5_thump <- gls(thump_avg ~ temperature, weights = vf5_thump, data = overall)
AIC(M00_thump, Mgls1_thump, Mgls3_thump, Mgls4_thump, Mgls5_thump)

# now we need to add that random variable

Mlme1_thump <- lme(thump_avg~temperature, weights = vf5_thump, data = overall, random = ~1|temperature)
AIC(Mgls5_thump, Mlme1_thump)
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(Mgls5_thump, add.smooth = FALSE, which = 1)
E <- resid(M0_thump)
hist(E, xlab = "Residuals", main = "")
plot(overall$thump_avg, E, xlab = "thump Duration",
     ylab = "Residuals")
par(op)

I1 <-order(temperature)
Mgls5_thump_pred <- predict(Mgls5_thump, se = TRUE, type = "response")
plot(thump_avg~temperature, data = overall, type = "p")
lines(temperature[I1], Mgls5_thump_pred+2*Mgls5_thump_pred,lty=2, type = "l", col = "blue")
lines(temperature[I1], Mgls5_thump_pred-2*Mgls5_thump_pred$se[I1],lty=2, col = "blue")
lines(temperature[I1], Mgls5_thump_pred, lty = 1, type = "l", col = "red")
abline(M0_sfreq, col = "green")
