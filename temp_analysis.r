overall <- read.csv("duration_summary_2014-06-12_16:43:35.csv", sep = ",", header = TRUE)
complete <-subset(overall, complete == TRUE)

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

buzzq1fit <- lm(buzz_q1~treatment, data=overall)
buzzq2fit <- lm(buzz_q2~treatment, data=overall)
buzzq3fit <- lm(buzz_q3~treatment, data=overall)
buzzq4fit <- lm(buzz_q4~treatment, data=overall)
buzzavgfit <- lm(buzz_avg~treatment, data=overall)
anovas_bq1 <-aov(buzzq1fit)
anovas_bq2 <-aov(buzzq2fit)
anovas_bq3 <-aov(buzzq3fit)
anovas_bq4 <-aov(buzzq4fit)
anovas_bavg <-aov(buzzavgfit)
TukeyHSD(anovas_bq1)
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


freqq1fit <- lm(buzzfreq_q1~treatment, data=overall)
freqq2fit <- lm(buzzfreq_q2~treatment, data=overall)
freqq3fit <- lm(buzzfreq_q3~treatment, data=overall)
freqq4fit <- lm(buzzfreq_q4~treatment, data=overall)
freqavgfit <- lm(buzzfreq_avg~treatment, data=overall)
anovas_fq1 <-aov(freqq1fit)
anovas_fq2 <-aov(freqq2fit)
anovas_fq3 <-aov(freqq3fit)
anovas_fq4 <-aov(freqq4fit)
anovas_favg <-aov(freqavgfit)
TukeyHSD(anovas_fq1)
TukeyHSD(anovas_fq2)
TukeyHSD(anovas_fq3)
TukeyHSD(anovas_fq4)
TukeyHSD(anovas_favg)


  
# Plots for all data (including those with missing data)  
bwplot(scrape_avg ~ treatment, data = overall,
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
  bwplot(thump_avg ~ treatment, data = overall,
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
  
  bwplot(buzz_avg ~ treatment, data = overall,
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
  
  
  bwplot(srates_avg ~ treatment, data = overall,
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

bwplot(buzzfreq_avg ~ treatment, data = overall,
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

  quarcols = list(col=c("black","black", "black", "black"),pch=c(16,16,16))
  bwplot(overall$srates_q1, overall$srates_q2, overall$srates_q3, overall$srates_q4, data = overall,
         xlab = "Quartile of Song", ylab = "Scrape Duration (s)", 
         fill = c("pink", "magenta", "red", "brown"),
         main = "Differences in Scrape Duration Through Time",
         par.settings = list(
           plot.symbol=cols,
           box.rectangle = cols,
           box.dot = cols,
           box.umbrella=cols 
           
         )
  )

quartilecols = c("light green", "chartreuse", "green", "dark green")  
coolqcols = c("light blue", "blue", "navy blue", "dark blue")
boxplot(overall$srates_q1, overall$srates_q2, overall$srates_q3, overall$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song", col = quartilecols, boxwex = .5, ylab = "scrapes/second")
boxplot(overall$scrape_q1, overall$scrape_q2, overall$scrape_q3, overall$scrape_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$thump_q1, overall$thump_q2, overall$thump_q3, overall$thump_q4, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Thump Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$buzz_q1, overall$buzz_q2, overall$buzz_q3, overall$buzz_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Buzz Duration During Song", col = quartilecols, boxwex = .5, ylab = "Duration (seconds)")
boxplot(overall$buzzfreq_q1, overall$buzzfreq_q2, overall$buzzfreq_q3, overall$buzzfreq_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Peak Buzz Frequency During Song", col = quartilecols, boxwex = .5, ylab = "Peak Frequency (Hz)")

overall.warm <-subset(overall, treatment == "warm")
overall.cool <-subset(overall, treatment == "cool") 
overall.rt <-subset(overall, treatment == "rt")
  
boxplot(overall.warm$srates_q1, overall.warm$srates_q2, overall.warm$srates_q3, overall.warm$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song (warm trials)", col = quartilecols, boxwex = .5)
  boxplot(overall.cool$srates_q1, overall.cool$srates_q2, overall.cool$srates_q3, overall.cool$srates_q4, data = overall, names = c(1, 2, 3, 4), xlab = "Quarter of Song", main = "Scrape Rate During Song (warm trials)", col = coolqcols, boxwex = .5) 
  
srates_date_avgfit <- lm(srates_avg~date, data=overall)
anovar_date <-aov(srates_date_avgfit)
TukeyHSD(anovar_date)

plot(overall$srates_avg~overall$weight, col = "yellow")
plot(overall$buzz_avg~overall$weight, col = "green")
plot(overall$scrape_avg~overall$weight, col = "blue")
points(overall$thump_avg~overall$weight, col = "red")
points(overall$srates_avg~overall$weight, col = "red")

# plotting features over temperature (continuous)
plot(overall$srates_avg~overall$temperature, col = "black", pch = 21, bg = "red", ylab = "Scrape Rate (scrapes/second", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Scrape Rate as a Function of Temperature")
model <- lm(srates_avg~temperature, data = overall)
abline(model, col = "red")
plot(overall$thump_avg~overall$temperature, col = "black", pch = 21, bg = "green", ylab = "Thump Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Thump Duration as a Function of Temperature")  
thumpmodel <- lm(thump_avg~temperature, data = overall)
abline(thumpmodel, col = "black")
plot(overall$scrape_avg~overall$temperature, col = "black", pch = 21, bg = "blue", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Scrape Duration as a Function of Temperature")

plot(overall$buzz_avg~overall$temperature, col = "black", pch = 21, bg = "purple", ylab = "Buzz Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Duration as a Function of Temperature")
buzzmodel <- lm(buzz_avg~temperature, data = overall)
abline(buzzmodel, col = "black")

plot(buzzfreq_avg~ct_width * weight, data = overall)
size <- (overall$ct_width * overall$weight)

plot(overall$buzzfreq_avg~size)
plot(overall$buzz_avg~size)
plot(overall$srates_avg~size)
plot(overall$scrape_avg~size)
plot(overall$thump_avg~size)
plot(overall$buzz_avg~size)
  
anova(lmer(srates_avg ~ treatment + (1|individual) + (1|date), data = complete))


testfit2 <- lmer(srates_avg ~ treatment + date + (1|individual), data = complete)

# Plots only using repeated measures data 
bwplot(scrape_avg ~ treatment, data = complete,
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
  
  
  bwplot(thump_avg ~ treatment, data = complete,
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
  
  
  bwplot(buzz_avg ~ treatment, data = complete,
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
  
  
  bwplot(srates_avg ~ treatment, data = complete,
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
