# Regression Analsyes

library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

setwd("~/projects/temp_trials/male_only/data")
details <- file.info(list.files(pattern="temp_vibration_data*"))
details <- details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
loadfile <- tail(files,1)
overall <-read.csv(loadfile)
overall <- read.csv("duration_summary_2014-06-16_23:54:07.csv", sep = ",", header = TRUE)
setwd("~/projects/temp_trials/male_only/analysis/")


feature_list <- c("scrape_avg", "thump_avg", "buzz_avg", "srates_avg", "fundfreq_avg", "srms_avg", "trms_avg", "brms_avg")
title_list <- c("Scrape Duration", "Thump Duration", "Buzz Duration", "Scrape Rate", "Fundamental Frequency", "Scrape Amplitude", "Thump Amplitude", "Buzz Amplitude")
label_list <-c("scrape duration (s)", "thump duration (s)", "buzz duration (s)", "scrape rate (scrapes/second)", "fundamental frequency (Hz)", "scrape amplitude(rms)", "thump amplitude (rms)", "buzz amplitude (rms)")
plotcols <-c("green", "red", "blue", "yellow", "purple", "orange", "maroon", "cyan")


plotvar = 1
sink("regression_summaries.txt", append=FALSE, split=TRUE)
pdf(file = "regression_plots.pdf")
with (overall,(
while(plotvar <= length(feature_list)) {
  
  # linear function
  linear_model <- lm(get(feature_list[plotvar])~temperature)  
  #abline(linear_model, col = "black", lwd = "3")
  cat(title_list[plotvar])
  cat("\n\nLinear Model\n")
  print(summary(linear_model))
  
  # setting up quadratic model
  tempvalues <- seq(15, 55, 0.1)
  temperature2 <- overall$temperature^2
  second_model <-lm(get(feature_list[plotvar]) ~ temperature + temperature2)
  second_predict <- predict(second_model,list(temperature=tempvalues, temperature2=tempvalues^2))
  #lines(tempvalues, quadscrapes, col = "red", lwd = 3)
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
  plotvar <- plotvar + 1
}
)
)
dev.off()
sink()
cat("We turned everything off.")


