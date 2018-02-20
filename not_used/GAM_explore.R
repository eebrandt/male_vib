#GAM with various vibratory characteristics
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

# Thump Duration
thump_avg <- overall$thump_avg
M0_thump <- lm(thump_avg~temperature)
M000_thump <- lm(thump_avg~1 + temperature)
M00_thump <- M1_thump <- gam(thump_avg~s(temperature, fx = FALSE, k=-1, bs = "cr"))

gvlma(M0_thump)

M1_thump <- gam(thump_avg~s(temperature, fx = FALSE, k=-1, bs = "cr")+ s(overall$individual, bs="re"))
M2_thump <- lme(thump_avg ~ temperature, data=overall, random = ~1|individual)
M25_thump <- lme(thump_avg ~ temperature, data = overall , random = ~1 + temperature|individual)
anova(M2_thump, M25_thump)
I1 <-order(temperature)
M1pred_thump <- predict(M1_thump, se = TRUE, type = "response")
M2pred_thump <- predict(M2_thump, se = TRUE, type = "response")
plot(temperature, thump_avg, type = "p")
lines(temperature[I1], M1pred_thump$fit[I1]+2*M1pred_thump$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_thump$fit[I1]-2*M1pred_thump$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_thump$fit[I1], lty = 1, type = "l", col = "red")
lines(temperature[I1], M2pred_thump, lty = 1, type = "l", col = "yellow")

lines(0:100,predict(M2_thump, newdata=data.frame(x=0:100),level=0), col="darkred", lwd=7)
abline(M0_thump, col = "green")
thing <- fitted(M2_thump)

AIC(M1_thump)
AIC(M00_thump)
AIC(M0_thump)
AIC(M2_thump)

#Buzz Duration
buzz_avg <- overall$buzz_avg
M0_buzz <- lm(temperature~buzz_avg)
summary(M0_buzz)
gvlma(M0_buzz)

M1_buzz <- gam(buzz_avg~s(temperature, fx = FALSE, k=-1, bs = "cr"))
plot(M1_buzz, se = TRUE)
I1 <-order(temperature)
M1pred_buzz <- predict(M1_buzz, se = TRUE, type = "response")
plot(temperature, buzz_avg, type = "p")
lines(temperature[I1], M1pred_buzz$fit[I1]+2*M1pred_buzz$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_buzz$fit[I1]-2*M1pred_buzz$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_buzz$fit[I1], lty = 1, type = "l", col = "red")
AIC(M1_buzz)
AIC(M0_buzz)

# Scrape Frequency
sfreq <- foverall$sfreq_avg
ftemp <- foverall$temperature
M0_sfreq <- lm(sfreq~ftemp)
summary(M0_sfreq)
gvlma(M0_sfreq)

M1_sfreq <- gam(sfreq~s(ftemp, fx = FALSE, k=-1, bs = "cr"))
plot(M1_sfreq, se = TRUE)
I1 <-order(ftemp)
M1pred_sfreq <- predict(M1_sfreq, se = TRUE, type = "response")
plot(ftemp, sfreq, type = "p")
lines(ftemp[I1], M1pred_sfreq$fit[I1]+2*M1pred_sfreq$se[I1],lty=2, type = "l", col = "blue")
lines(ftemp[I1], M1pred_sfreq$fit[I1]-2*M1pred_sfreq$se[I1],lty=2, col = "blue")
lines(ftemp[I1], M1pred_sfreq$fit[I1], lty = 1, type = "l", col = "red")
abline(M0_sfreq, col = "green")
AIC(M1_sfreq)

AIC(M0_sfreq)


#Scrape Rate
srate <- overall$srates_avg
M0_srate <- lm(temperature~srate)
summary(M0_srate)
gvlma(M0_srate)

M1_srate <- gam(srate~s(temperature, fx = FALSE, k=-1, bs = "cr"))
plot(M1_srate, se = TRUE)
M2_srate <- lme(srate ~ temperature, data=overall, random = ~1|individual)
I1 <-order(temperature)
M1pred_srate <- predict(M1_srate, se = TRUE, type = "response")
M2pred_srate <- predict(M2_srate, se = TRUE, type = "response")
plot(temperature, srate, type = "p")
lines(temperature[I1], M1pred_srate$fit[I1]+2*M1pred_srate$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_srate$fit[I1]-2*M1pred_srate$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_srate$fit[I1], lty = 1, type = "l", col = "red")
lines(temperature[I1], M2pred_srate, lty = 1, type = "l", col = "yellow")
AIC(M1_srate)
AIC(M0_srate)
AIC(M2_srate)


#Scrape Amplitude
srms <- overall$srms_avg
M0_srms <- lm(srms~temperature)
plot(temperature, srms)
abline(M0_srms)
#plot(M0_srms)
summary(M0_srms)
gvlma(M0_srms)

M1_srms <- gam(srms~s(temperature, fx = FALSE, k=-1, bs = "cr"))
plot(M1_srms, se = TRUE)
I1 <-order(temperature)
M1pred_srms <- predict(M1_srms, se = TRUE, type = "response")
plot(temperature, srms, type = "p")
lines(temperature[I1], M1pred_srms$fit[I1]+2*M1pred_srms$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_srms$fit[I1]-2*M1pred_srms$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_srms$fit[I1], lty = 1, type = "l", col = "red")
abline(M0_srms, col = "green")
AIC(M1_srms)
AIC(M0_srms)

#Thump Amplitude
trms <- overall$trms_avg
M0_trms <- lm(trms~temperature)
plot(temperature, trms)
abline(M0_trms)
#plot(M0_trms)
summary(M0_trms)
gvlma(M0_trms)

M1_trms <- gam(trms~s(temperature, fx = FALSE, k=-1, bs = "cr"))
#plot(M1_trms, se = TRUE)
I1 <-order(temperature)
M1pred_trms <- predict(M1_trms, se = TRUE, type = "response")
plot(temperature, trms, type = "p")
lines(temperature[I1], M1pred_trms$fit[I1]+2*M1pred_trms$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_trms$fit[I1]-2*M1pred_trms$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_trms$fit[I1], lty = 1, type = "l", col = "red")
abline(M0_trms, col = "green")
AIC(M1_trms)
AIC(M0_trms)

#Buzz Amplitude
brms <- overall$brms_avg
M0_brms <- lm(temperature~brms)
plot(temperature, brms)

#plot(M0_brms)
summary(M0_brms)
gvlma(M0_brms)

M1_brms <- gam(brms~s(temperature, fx = FALSE, k=-1, bs = "cr"))
plot(M1_brms, se = TRUE)
I1 <-order(temperature)
M1pred_brms <- predict(M1_brms, se = TRUE, type = "response")
plot(temperature, brms, type = "p")
lines(temperature[I1], M1pred_brms$fit[I1]+2*M1pred_brms$se[I1],lty=2, type = "l", col = "blue")
lines(temperature[I1], M1pred_brms$fit[I1]-2*M1pred_brms$se[I1],lty=2, col = "blue")
lines(temperature[I1], M1pred_brms$fit[I1], lty = 1, type = "l", col = "red")
abline(M0_brms, col = "green")
AIC(M1_brms)
AIC(M0_brms)

