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
anovas_savg <-aov(scrape_avgfit)

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
#TukeyHSD(anovas_tq1)
#TukeyHSD(anovas_tq2)
#TukeyHSD(anovas_tq3)
#TukeyHSD(anovas_tq4)
#TukeyHSD(anovas_tavg)

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
#TukeyHSD(anovas_bq2)
#TukeyHSD(anovas_bq3)
#TukeyHSD(anovas_bq4)
#TukeyHSD(anovas_bavg)

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
#TukeyHSD(anovas_rq1)
#TukeyHSD(anovas_rq2)
#TukeyHSD(anovas_rq3)
#TukeyHSD(anovas_rq4)
#TukeyHSD(anovas_ravg)





#fundq1fit <- lm(fundfreq_q1~treatment, data=overall)
sfreq_q1 <- lm(sfreq_q1~treatment, data=overall)
sfreq_q2 <- lm(sfreq_q2~treatment, data=overall)
sfreq_q3 <- lm(sfreq_q3~treatment, data=overall)
sfreq_q4 <- lm(sfreq_q4~treatment, data=overall)
sfreq_avgfit <- lm(sfreq_avg~treatment, data=overall)

#anovas_fq1 <-aov(fundq1fit)
anovas_fsq1 <-aov(sfreq_q1)
anovas_fsq2 <-aov(sfreq_q2)
anovas_fsq3 <-aov(sfreq_q3)
anovas_fsq4 <-aov(sfreq_q4)
anovas_fsavg <-aov(sfreq_avgfit)

#fundq1fit <- lm(fundfreq_q1~treatment, data=overall)
tfreq_q1 <- lm(tfreq_q1~treatment, data=overall)
tfreq_q2 <- lm(tfreq_q2~treatment, data=overall)
tfreq_q1 <- lm(tfreq_q3~treatment, data=overall)
tfreq_q4 <- lm(tfreq_q4~treatment, data=overall)
tfreq_avgfit <- lm(tfreq_avg~treatment, data=overall)

#anovas_fq1 <-aov(fundq1fit)
anovas_ftq1 <-aov(sfreq_q1)
anovas_ftq2 <-aov(sfreq_q2)
anovas_ftq3 <-aov(sfreq_q2)
anovas_ftq4 <-aov(sfreq_q3)
anovas_ftavg <-aov(sfreq_avgfit)


#fundq1fit <- lm(fundfreq_q1~treatment, data=overall)
bfreq_2fit <- lm(bfreq_q2~treatment, data=overall)
bfreq_3fit <- lm(bfreq_q3~treatment, data=overall)
bfreq_4fit <- lm(bfreq_q4~treatment, data=overall)
bfreq_avgfit <- lm(bfreq_avg~treatment, data=overall)
#anovas_fq1 <-aov(fundq1fit)
anovas_fq2 <-aov(bfreq_2fit)
anovas_fq3 <-aov(bfreq_3fit)
anovas_fq4 <-aov(bfreq_4fit)
anovas_favg <-aov(bfreq_avgfit)

sink("tukey_summaries.txt", append=FALSE, split=TRUE)
cat("Scrape Duration\n")
print(TukeyHSD(anovas_savg))
cat("Thump Duration\n")
print(TukeyHSD(anovas_tavg))
cat("Buzz Duration\n")
print(TukeyHSD(anovas_bavg))
cat("Scrape Rates\n")
print(TukeyHSD(anovas_ravg))
cat("Scrape Peak Frequency")
print(TukeyHSD(anovas_fsavg))
cat("Thump Peak Frequency")
print(TukeyHSD(anovas_ftavg))
cat("Buzz Fundamentals\n")
print(TukeyHSD(anovas_favg))
print(TukeyHSD(anovas_fsavg))
print(TukeyHSD(anovas_ftavg))
sink()

pdf(file = "Tukey_plots.pdf") 

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

sfreq_all <- bwplot(sfreq_avg ~ treatment, data = overall,
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

tfreq_all <- bwplot(tfreq_avg ~ treatment, data = overall,
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

bfreq_all <- bwplot(bfreq_avg ~ treatment, data = overall,
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

srms_all <- bwplot(srms_avg ~ treatment, data = overall,
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
trms_all <- bwplot(trms_avg ~ treatment, data = overall,
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

rm_bfreq <- bwplot(bfreq_avg ~ treatment, data = complete,
            xlab = "Treatment", ylab = "Fundamental Frequency (Hz)", 
            fill = rmcols,
            main = "Differences in Fundamental Frequency For Three Temperature Treatments (repeated measures)",
            par.settings = list(
                plot.symbol=cols,
                box.rectangle = cols,
                box.dot = cols,
                box.umbrella=cols 
                     
           )
)
plot(rm_bfreq)
dev.off()
