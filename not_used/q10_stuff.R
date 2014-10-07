# This script reads in the "temp_vibration_data" csv and:
#1) Generates anovas, anovas with treatment/individual error, and tukey tests
#2) Makes boxplots for average treatment data
#3) Makes scatterplots for all data
#4) Performs logistic regression for scatterplot above
#5) Calculates Q(10) value for all data above
#6) Prints R squared values for all regressions (linear, second order, third order, logarithmic)


library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

setwd("~/projects/temp_trials/male_only/data")
complete <-subset(overall, complete == TRUE)
details <- file.info(list.files(pattern="temp_vibration_data*"))
details <- details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
loadfile <- tail(files,1)
overall <-read.csv(loadfile)
setwd("~/projects/temp_trials/male_only/analysis/")
cols = list(col=c("black","black", "black"),pch=c(16,16,16))
rmcols = c("steel blue", "gold", "magenta")

feature_list <- c("scrape_avg", "thump_avg", "buzz_avg", "srates_avg", "sfreq_avg", "tfreq_avg", "bfreq_avg","srms_avg", "trms_avg", "brms_avg")
title_list <- c("Scrape Duration", "Thump Duration", "Buzz Duration", "Scrape Rate", "Scrape Peak Frequency", "Thump Peak Frequency","Buzz Fundamental Frequency", "Scrape Amplitude", "Thump Amplitude", "Buzz Amplitude")
label_list <-c("scrape duration (s)", "thump duration (s)", "buzz duration (s)", "scrape rate (scrapes/second)", "peak frequency(Hz)", "peak frequency (Hz)", "fundamental frequency (Hz)", "scrape amplitude(rms)", "thump amplitude (rms)", "buzz amplitude (rms)")
plotcols <-c("blue", "dark blue", "cyan", "yellow", "light green", "green", "chartreuse", "pink", "red", "maroon")


plotvar = 1
sink("regression_summaries.txt", append=FALSE, split=TRUE)
pdf(file = "male_temp_plots.pdf")
with (overall,(
while(plotvar <= length(feature_list)) {
  cat(title_list[plotvar])
  # linear function
  linear_model <- lm(get(feature_list[plotvar])~temperature)  
  #abline(linear_model, col = "black", lwd = "3")
  cat("\n\nLinear Model\n")
  print(summary(linear_model))
  
  # setting up quadratic model
  tempvalues <- seq(15, 55, 0.1)
  temperature2 <- overall$temperature^2
  second_model <-lm(get(feature_list[plotvar]) ~ temperature + temperature2)
  second_predict <- predict(second_model,list(temperature=tempvalues, temperature2=tempvalues^2))
  cat("\nSecond Order Model")
  print(summary(second_model))
  
  # setting up third order function
  temperature3 <- overall$temperature^3
  third_model <-lm(get(feature_list[plotvar]) ~ temperature + temperature2 + temperature3)
  third_predict <- predict(third_model,list(temperature=tempvalues, temperature2=tempvalues^2, temperature3 = tempvalues^3 ))
  #lines (tempvalues, third_predict, col="purple", lwd = 3)
  cat("\nThird Order Model\n")
  print(summary(third_model))
  
  #setting up simple logarithmic model
  log_model <- lm(log(get(feature_list[plotvar])) ~ temperature)
  log_predict <- exp(predict(log_model,list(temperature=tempvalues)))
  
  
  R_squared <- summary(log_model)$r.squared
  #coefs_scrape <- list( coef1 = coef(log_model)[1], coef2 = coef(linear_scrape)[2])
  cat("Logarithmic Model\n")
  print(summary(log_model))
  cat(paste(title_list[plotvar],"R squared =", round(R_squared, 3), "\n"))
  
  e <-exp(1)
  coef1 <- log_model$coefficients[1]
  coef2 <-log_model$coefficients[2]
  q10_25 <- e ^ (coef2 * 25 + coef1)
  q10_35 <- e ^ (coef2 * 35 + coef1)
  q10_total = (q10_35/q10_25)
  
  if (q10_total < 1) {q10_total <- q10_25/q10_35}
  
  plot(get(feature_list[plotvar])~temperature,  
       main = bquote("Average" ~ .(title_list[plotvar]) ~ "for all Trials as a Function of Temperature"),  
       sub = bquote("Q(10) = " ~ .(round(q10_total, 3))), font.sub=3,
       col = "black", pch = 21, bg = plotcols[plotvar], ylab = label_list[plotvar], xlab = expression(paste("Temperature (", degree,"C)")),
      )
  lines(tempvalues, log_predict, col = "black", lwd = 3)
  
  cat(paste("Q(10) = ", round(q10_total, 3), "\n"))
  cat("________________________________________________\n\n")

  
  # non-rm anovas
  # linear fits, anovas and tukeys for each feature

  #q1fit <- lm(get(feature_list[plotvar])~treatment, data=overall)
  #q2fit <- lm(scrape_q2~treatment, data=overall)
  #q3fit <- lm(scrape_q3~treatment, data=overall)
  #q4fit <- lm(scrape_q4~treatment, data=overall)
  avgfit <- lm(get(feature_list[plotvar])~treatment, data = overall)
  anova_avg <-aov(avgfit)
  print(summary(anova_avg))
  # repeated measures anova
  anova_rm = aov(get(feature_list[plotvar])~treatment + Error(individual/treatment))
  summary(anova_rm)
  # tukey (non-rm data)
  cat ("Repeated Measures ANOVA")
  print(TukeyHSD(anova_avg))

  # Let's plot it!
  # all data
  all <- bwplot(get(feature_list[plotvar]) ~ treatment,
            xlab = "Treatment", ylab = label_list[plotvar], 
            fill = c("blue", "yellow", "red"),
            main = paste("Average", title_list[plotvar], "for each Temperature Treatment \n(all data)"),
            par.settings = list(
              plot.symbol=cols,
              box.rectangle = cols,
              box.dot = cols,
              box.umbrella=cols     
                  )
  )

  plot(all)

  plotvar <- plotvar + 1  
}
)
)

sink()
plotvar = 1
with (complete,(
  while(plotvar <= length(feature_list)) {
    rm <- bwplot(get(feature_list[plotvar]) ~ treatment,
            xlab = "Treatment", ylab = label_list[plotvar], 
            fill = c("steel blue", "gold", "magenta"),
            main = paste("Average", title_list[plotvar], "for each Temperature Treatment \n(repeated measures)"),
            par.settings = list(
              plot.symbol=cols,
              box.rectangle = cols,
              box.dot = cols,
              box.umbrella=cols     
               )
  )
    plot(rm)    
plotvar <- plotvar + 1    
}
)
)      
  
 
dev.off()
#dev.off()
cat("We turned everything off.")
