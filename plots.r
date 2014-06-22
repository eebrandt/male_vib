library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

setwd("~/projects/temp_trials/male_only/data")
details <- file.info(list.files(pattern="temp_vibration_data*"))
details <- details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
loadfile <- tail(files,1)
overall <-read.csv(loadfile, sep = ",", header = TRUE)
setwd("~/projects/temp_trials/male_only/analysis/")
complete <-subset(overall, complete == TRUE)
overall.warm <-subset(overall, treatment == "warm")
overall.cool <-subset(overall, treatment == "cool") 
overall.rt <-subset(overall, treatment == "rt")
coolqcols = c("black", "dark blue", "blue", "light blue")
rmcols = c("steel blue", "gold", "magenta")
cols = list(col=c("black","black", "black"),pch=c(16,16,16))

# linear fits, anovas and tukeys for each feature
scrapeq1fit <- lm(scrape_q1~treatment, data=overall)
scrapeq2fit <- lm(scrape_q2~treatment, data=overall)
scrapeq3fit <- lm(scrape_q3~treatment, data=overall)
scrapeq4fit <- lm(scrape_q4~treatment, data=overall)
scrape_avgfit <- lm(scrape_avg~treatment, data=overall)
anovas_q1 <-aov(scrapeq1fit)
TukeyHSD(anovas_q1)

thumpq1fit <- lm(thump_q1~treatment, data=overall)
thumpq2fit <- lm(thump_q2~treatment, data=overall)
thumpq3fit <- lm(thump_q3~treatment, data=overall)
thumpq4fit <- lm(thump_q4~treatment, data=overall)
thumpavgfit <- lm(thump_avg~treatment, data=overall)
anovas_tq1 <-aov(thumpq1fit)
anovas_tq2 <-aov(thumpq2fit)
anovas_tq3 <-aov(thumpq3fit)
anovas_tq4 <-aov(thumpq4fit)
anovas_tavg <-aov(thumpavgfit)
TukeyHSD(anovas_tq1)
TukeyHSD(anovas_tq2)
TukeyHSD(anovas_tq3)
TukeyHSD(anovas_tq4)
TukeyHSD(anovas_tavg)

#buzzq1fit <- lm(buzz_q1~treatment, data=overall)
buzzq2fit <- lm(buzz_q2~treatment, data=overall)
buzzq3fit <- lm(buzz_q3~treatment, data=overall)
buzzq4fit <- lm(buzz_q4~treatment, data=overall)
buzzavgfit <- lm(buzz_avg~treatment, data=overall)
#anovas_bq1 <-aov(buzzq1fit)
anovas_bq2 <-aov(buzzq2fit)
anovas_bq3 <-aov(buzzq3fit)
anovas_bq4 <-aov(buzzq4fit)
anovas_bavg <-aov(buzzavgfit)
#TukeyHSD(anovas_bq1)
TukeyHSD(anovas_bq2)
TukeyHSD(anovas_bq3)
TukeyHSD(anovas_bq4)
TukeyHSD(anovas_bavg)

sratesq1fit <- lm(srates_q1~treatment, data=overall)
sratesq2fit <- lm(srates_q2~treatment, data=overall)
sratesq3fit <- lm(srates_q3~treatment, data=overall)
sratesq4fit <- lm(srates_q4~treatment, data=overall)
sratesavgfit <- lm(srates_avg~treatment, data=overall)
anovas_rq1 <-aov(sratesq1fit)
anovas_rq2 <-aov(sratesq2fit)
anovas_rq3 <-aov(sratesq3fit)
anovas_rq4 <-aov(sratesq4fit)
anovas_ravg <-aov(sratesavgfit)
TukeyHSD(anovas_rq1)
TukeyHSD(anovas_rq2)
TukeyHSD(anovas_rq3)
TukeyHSD(anovas_rq4)
TukeyHSD(anovas_ravg)

#fundq1fit <- lm(fundfreq_q1~treatment, data=overall)
fundq2fit <- lm(fundfreq_q2~treatment, data=overall)
fundq3fit <- lm(fundfreq_q3~treatment, data=overall)
fundq4fit <- lm(fundfreq_q4~treatment, data=overall)
fundavgfit <- lm(fundfreq_avg~treatment, data=overall)
#anovas_fq1 <-aov(fundq1fit)
anovas_fq2 <-aov(fundq2fit)
anovas_fq3 <-aov(fundq3fit)
anovas_fq4 <-aov(fundq4fit)
anovas_favg <-aov(fundavgfit)
#TukeyHSD(anovas_fq1)
TukeyHSD(anovas_fq2)
TukeyHSD(anovas_fq3)
TukeyHSD(anovas_fq4)
TukeyHSD(anovas_favg)

pdf(file = "plotzes!") 

# Plots for all data (including those with missing data)  
scrape_dur_all <- bwplot(scrape_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Scrape Duration For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
        
       )
)
plot(scrape_dur_all)

thump_dur_all <- bwplot(thump_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Thump Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Thump Duration For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(thump_dur_all)

buzz_dur_all <- bwplot(buzz_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Buzz Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Buzz Duration For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(buzz_dur_all)

srates_all <- bwplot(srates_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Scrapes/Second", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Scrape Rate For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(srates_all)
bwplot(fundfreq_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Peak Frequency (Hz)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Peak Buzz Frequency For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)

# rms stuff

srms_all <- bwplot(srms_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Scrape Amplitude (rms) For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(srms_all)
bwplot(trms_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Thump Amplitude (rms) For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)

brms_all <- bwplot(brms_avg ~ treatment, data = overall,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Buzz Amplitude (rms) For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(brms_all)

quartilecols = c("light green", "chartreuse", "green", "dark green")  

boxplot(overall$srates_q1, overall$srates_q2, overall$srates_q3, overall$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song", col = quartilecols, boxwex = .5, ylab = "scrapes/second")
boxplot(overall$scrape_q1, overall$scrape_q2, overall$scrape_q3, overall$scrape_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$thump_q1, overall$thump_q2, overall$thump_q3, overall$thump_q4, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Thump Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$buzz_q1, overall$buzz_q2, overall$buzz_q3, overall$buzz_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Buzz Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$fundfreq_q1, overall$fundfreq_q2, overall$fundfreq_q3, overall$fundfreq_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Fundamental Buzz Frequency During Song", col = quartilecols, boxwex = .5, ylab = "Fundamental Frequency (Hz)")

# boxplots only
plot(overall.warm$fundfreq_avg~overall.warm$temperature)
linfundwarm <-(lm(fundfreq_avg~temperature, data = overall))
abline(linfundwarm, col = "red")
plot(overall.cool$fundfreq_avg~overall.cool$temperature)
linfundcool <- (lm(fundfreq_avg~temperature, data = overall.cool))
abline(linfundcool, col = "blue")
plot(overall.rt$fundfreq_avg~overall.rt$temperature)

boxplot(overall.warm$srates_q1, overall.warm$srates_q2, overall.warm$srates_q3, overall.warm$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song (warm trials)", col = quartilecols, boxwex = .5)
boxplot(overall.cool$srates_q1, overall.cool$srates_q2, overall.cool$srates_q3, overall.cool$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song (warm trials)", col = coolqcols, boxwex = .5) 

plot(overall$srates_avg~overall$weight, col = "yellow")
plot(overall$buzz_avg~overall$weight, col = "green")
plot(overall$scrape_avg~overall$weight, col = "blue")
points(overall$thump_avg~overall$weight, col = "red")
points(overall$srates_avg~overall$weight, col = "red")

#plot(overall$weight ~ overall$ct_width)
#plot(fundfreq_avg~ct_width * weight, data = overall)
#size <- (overall$ct_width * overall$weight)

#plot(overall$fundfreq_avg~size)
#plot(overall$buzz_avg~size)
#plot(overall$srates_avg~size)
#plot(overall$scrape_avg~size)
#plot(overall$thump_avg~size)
#plot(overall$buzz_avg~size)

plot(overall$brms_avg~overall$temperature)
plot(overall$srms_avg~overall$temperature)
plot(overall$trms_avg~overall$temperature)
plot(overall$trms_avg~overall$weight)

plot(overall$trms_avg~overall$weight)
lin_trms <-lm(trms_avg~weight, data = overall)
abline(lin_trms, col = "red")

plot(overall$srms_avg~overall$weight)
lin_srms <-lm(srms_avg~weight, data = overall)
abline(lin_srms, col = "red")

plot(overall$brms_avg~overall$weight)
lin_brms <-lm(brms_avg~weight, data = overall)
abline(lin_brms, col = "red")

yrange_trms<-range(c(overall.warm$trms_avg, overall.cool$trms_avg, overall.rt$trms_avg))
xrange_trms <- range(c(overall.warm$weight, overall.cool$weight, overall.rt$weight), na.rm=TRUE)
plot(overall.warm$trms_avg~overall.warm$weight, xlim = xrange_trms, ylim = yrange_trms, col = "black", pch = 21, bg = "red", ylab = "Thump RMS", xlab = "Weight (g)", main = "Thump RMS vs. Weight")
points(overall.cool$trms_avg~overall.cool$weight, col = "black", pch = 21, bg ="blue")
points(overall.rt$trms_avg~overall.rt$weight, col = "black", pch = 21, bg = "yellow")
lin_trms <- lm(trms_avg~weight, data = overall)
abline(lin_trms, col = "black")

yrange_srms<-range(c(overall.warm$srms_avg, overall.cool$srms_avg, overall.rt$srms_avg))
xrange_srms <- range(c(overall.warm$weight, overall.cool$weight, overall.rt$weight), na.rm=TRUE)
plot(overall.warm$srms_avg~overall.warm$weight, xlim = xrange_srms, ylim = yrange_srms, col = "black", pch = 21, bg = "red", ylab = "Scrape RMS", xlab = "Weight (g)", main = "Scrape RMS vs. Weight")
points(overall.cool$srms_avg~overall.cool$weight, col = "black", pch = 21, bg ="blue")
points(overall.rt$srms_avg~overall.rt$weight, col = "black", pch = 21, bg = "yellow")
lin_srms <- lm(srms_avg~weight, data = overall)
abline(lin_srms, col = "black")

yrange_brms<-range(c(overall.warm$brms_avg, overall.cool$brms_avg, overall.rt$brms_avg))
xrange_brms <- range(c(overall.warm$weight, overall.cool$weight, overall.rt$weight), na.rm=TRUE)
plot(overall.warm$brms_avg~overall.warm$weight, col = "black", pch = 21, bg = "red", ylab = "Buzz RMS", xlab = "Weight (g)", main = "Buzz RMS vs. Weight")
points(overall.cool$brms_avg~overall.cool$weight, col = "black", pch = 21, bg ="blue")
points(overall.rt$brms_avg~overall.rt$weight, col = "black", pch = 21, bg = "yellow")
lin_brms <- lm(brms_avg~weight, data = overall)
abline(lin_brms, col = "black")

anova(lmer(srates_avg ~ treatment + (1|individual) + (1|date), data = complete))

# generalized mixed model here:
testfit2 <- lmer(srates_avg ~ treatment + date + (1|individual), data = complete)

# Plots only using repeated measures data 
rm_s <- bwplot(scrape_avg ~ treatment, data = complete,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = rmcols,
       main = "Differences in Scrape Duration For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(rm_s)

rm_t <- bwplot(thump_avg ~ treatment, data = complete,
       xlab = "Treatment", ylab = "Thump Duration (s)", 
       fill = rmcols,
       main = "Differences in Thump Duration For Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(rm_t)

rm_b <- bwplot(buzz_avg ~ treatment, data = complete,
       xlab = "Treatment", ylab = "Buzz Duration (s)", 
       fill = rmcols,
       main = "Differences in Buzz Duration For Three Temperature Treatments (repeated measures)",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(rm_b)

rm_srate <- bwplot(srates_avg ~ treatment, data = complete,
       xlab = "Treatment", ylab = "Scrapes/Second", 
       fill = rmcols,
       main = "Differences in Scrape Rate For Three Temperature Treatments (repeated measures)",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(rm_srate)
dev.off()
