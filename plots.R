#Runs repeated measures-friendly ANOVAS and multcomp Tukey post-hoc tests.

library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("multcomp", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

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
plot(complete)

# linear fits, anovas and tukeys for each feature (duration)
scrapeavg_lme <- lme(scrape_avg ~ treatment, data=complete, random = ~1|individual)
scrapeavg_lin <- lm(scrape_avg ~ treatment, data = complete)
AIC(scrapeavg_lme)
AIC(scrapeavg_lin)
anovas_savg <- anova(scrapeavg_lme)

thumpavg_lme <- lme(thump_avg ~ treatment, data=complete, random = ~1|individual)
thumpavg_lin <- lm(thump_avg ~ treatment, data = complete)
AIC(thumpavg_lme)
AIC(thumpavg_lin)
anovas_tavg <- anova(thumpavg_lme)

buzzavg_lme <- lme(buzz_avg ~ treatment, data=complete, random = ~1|individual)
buzzavg_lin <- lm(buzz_avg ~ treatment, data = complete)
AIC(buzzavg_lme)
AIC(buzzavg_lin)
anovas_bavg <- anova(buzzavg_lme)

# scrape rate
sravg_lme <- lme(srates_avg ~ treatment, data=complete, random = ~1|individual)
anovas_ravg <-anova(sravg_lme)

# frequency stuff
sfreq_lme <- lme(sfreq_avg ~ treatment, data=complete, random = ~1|individual)
anovas_fsavg <-anova(sfreq_lme)

tfreq_lme <- lme(tfreq_avg ~ treatment, data=complete, random = ~1|individual)
anovas_ftavg <-anova(tfreq_lme)

bfreq_lme <- lme(bfreq_avg ~ treatment, data=complete, random = ~1|individual)
anovas_favg <-anova(bfreq_lme)

# rms values
srms_lme <- lme(srms_avg ~ treatment, data=complete, random = ~1|individual)
anovasrmsavg <-anova(srms_lme)

trms_lme <- lme(trms_avg ~ treatment, data=complete, random = ~1|individual)
anovatrmsavg <-anova(trms_lme)

brms_lme <- lme(brms_avg ~ treatment, data=complete, random = ~1|individual)
anovabrmsavg <-anova(brms_lme)

# Put Tukeys in text document
sink("tukey_summaries_final.txt", append=FALSE, split=TRUE)
cat("Scrape Duration\n")
print(anovas_savg)
print(summary(glht(scrapeavg_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Thump Duration\n")
print(anovas_tavg)
print(summary(glht(thumpavg_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Buzz Duration\n")
print(anovas_bavg)
print(summary(glht(buzzavg_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Scrape Rates\n")
print(anovas_ravg)
print(summary(glht(sravg_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Scrape Peak Frequency")
print(anovas_fsavg)
print(summary(glht(sfreq_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Thump Peak Frequency")
print(anovas_ftavg)
print(summary(glht(tfreq_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Buzz Fundamentals\n")
print(anovas_favg)
print(summary(glht(bfreq_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Scrape RMS\n")
print(anovasrmsavg)
print(summary(glht(srms_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Thump RMS\n")
print(anovatrmsavg)
print(summary(glht(trms_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
cat("Buzz RMS\n")
print(anovabrmsavg)
print(summary(glht(brms_lme, linfct=mcp(treatment = "Tukey")), test = adjusted(type = "bonferroni")))
sink()

pdf(file = "Tukey_plots.pdf") 

# Plots for all data (including those with missing data)  
scrape_dur_all <- bwplot(scrape_avg ~ treatment, data = complete,
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

thump_dur_all <- bwplot(thump_avg ~ treatment, data = complete,
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

buzz_dur_all <- bwplot(buzz_avg ~ treatment, data = complete,
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

srates_all <- bwplot(srates_avg ~ treatment, data = complete,
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

sfreq_all <- bwplot(sfreq_avg ~ treatment, data = complete,
              xlab = "Treatment", ylab = "Fundamental Frequency (Hz)",
              fill = c("blue", "yellow", "red"),
              main = "Differences in Peak Scrape Frequency for Three Temperature Treatments",
              par.settings = list(
                  plot.symbol=cols,
                  box.rectangle = cols,
                  box.dot = cols,
                  box.umbrella=cols 
                      
                  )
)

plot(sfreq_all)

tfreq_all <- bwplot(tfreq_avg ~ treatment, data = complete,
                    xlab = "Treatment", ylab = "Fundamental Frequency (Hz)",
                    fill = c("blue", "yellow", "red"),
                    main = "Differences in Peak Thump Frequency for Three Temperature Treatments",
                    par.settings = list(
                      plot.symbol=cols,
                      box.rectangle = cols,
                      box.dot = cols,
                      box.umbrella=cols 
                      
                    )
                  )
plot(tfreq_all)

bfreq_all <- bwplot(bfreq_avg ~ treatment, data = complete,
                    xlab = "Treatment", ylab = "Fundamental Frequency (Hz)", 
                    fill = c("blue", "yellow", "red"),
                    main = "Differences in Fundamental Buzz Frequency For Three Temperature Treatments",
                    par.settings = list(
                      plot.symbol=cols,
                      box.rectangle = cols,
                      box.dot = cols,
                      box.umbrella=cols 
                      
                    )
)

plot(bfreq_all)
# rms stuff

srms_all <- bwplot(srms_avg ~ treatment, data = complete,
       xlab = "Treatment", ylab = "Scrape Duration (s)", 
       fill = c("blue", "yellow", "red"),
       main = "Differences in Scrape Amplitude (rms) for Three Temperature Treatments",
       par.settings = list(
         plot.symbol=cols,
         box.rectangle = cols,
         box.dot = cols,
         box.umbrella=cols 
         
       )
)
plot(srms_all)
trms_all <- bwplot(trms_avg ~ treatment, data = complete,
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
plot(trms_all)
brms_all <- bwplot(brms_avg ~ treatment, data = complete,
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

dev.off()
