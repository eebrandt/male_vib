source("/home/eebrandt/projects/temp_trials/male_only/src/plots.R")
source("/home/eebrandt/projects/temp_trials/male_only/src/q10_stuff.R")

Rscript 
# Things to Discuss with Chris:
# 1) Explain current layout
#   - show the sound file
#   - show the Tukey results
#   % ask about repeated measures
# 2) Regression Stuff
#   - show current plots
#   % ask about regressions
# 3) Markov Chain Stuff
#   % ask about higher-order analysis
# 4) Female Stuff
#   % ask about repeated trials.  Can I use any of this data to show recovery?

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
complete <-subset(overall, complete == TRUE)
coolqcols = c("black", "dark blue", "blue", "light blue")
rmcols = c("steel blue", "gold", "magenta")
cols = list(col=c("black","black", "black"),pch=c(16,16,16))

pdf(file = "regression_plots.pdf")
powerguess <- c(.5, 1, 2, 2.5)

#(1) Scrape Duration

# linear model
linear_scrape <- lm(scrape_avg~temperature, data = overall)

coefs_scrape <- list( coef1 = coef(linear_scrape)[1], coef2 = coef(linear_scrape)[2])
plot(scrape_avg~temperature, data = overall,  main = "Average scrape durations for all trials as a function of temperature", sub = bquote("scrape duration =" ~ .(coefs_scrape$coef2) ~ "* temp +" ~ .(coefs_scrape$coef1) ), col = "black", pch = 21, bg = "green", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")))
abline(linear_scrape, col = "black", lwd = "3")

# setting up quadratic model
tempvalues <- seq(15, 55, 0.1)
temperature2 <- overall$temperature^2
quadratic_scrape <-lm(scrape_avg ~ temperature + temperature2, data = overall)
quadscrapes <- predict(quadratic_scrape,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadscrapes, col = "red", lwd = 3)

# setting up third order function
temperature3 <- overall$temperature^3
#setting up simple logarithmic model
exp_scrape <- lm(log(scrape_avg) ~ temperature, data = overall)
exp_scrapes <- exp(predict(exp_scrape,list(temperature=tempvalues)))
lines(tempvalues, exp_scrapes, col = "blue", lwd = 3)

#(5) Thump Duration

thump_lin <- lm(thump_avg~temperature, data = overall)
coefs_thump <-list(coef1 = round(coef(thump_lin)[1], digits = 4), coef2 = round(coef(thump_lin)[2], digits = 4))
plot(overall$thump_avg~overall$temperature, col = "black", pch = 21, bg = "maroon", ylab = "Thump Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Thump Duration as a Function of Temperature", sub = bquote("thump duration =" ~ .(coefs_thump$coef2) ~ "* temp +" ~ .(coefs_thump$coef1) ))
abline(thump_lin, col = "black", lwd = "3")

q10_thump <- ((coefs_thump$coef2 * 25)+ coefs_thump$coef1)/((coefs_thump$coef2 * 35)+ coefs_thump$coef1)
mtext(bquote(paste("Q"[10]* " =  ", .(round(q10_thump, digits = 4)))))

quadratic_thump <-lm(thump_avg ~ temperature + temperature2, data = overall)
quadthumps <- predict(quadratic_thump,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadthumps, col = "red", lwd = 3)

# logarithmic model
exp_thump <- lm(log(thump_avg) ~ temperature, data = overall)
exp_thumps <- exp(predict(exp_thump,list(temperature=tempvalues)))
lines(tempvalues, exp_thumps, col = "blue", lwd = 3)

#(4) Buzz Duration

buzz_lin <- lm(buzz_avg~temperature, data = overall)
coefs_buzz <-list(coef1 = round(coef(buzz_lin)[1], digits = 4), coef2 = round(coef(buzz_lin)[2], digits = 4))
plot(overall$buzz_avg~overall$temperature, col = "black", pch = 21, bg = "orange", ylab = "Buzz Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Duration as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_buzz$coef2) ~ "* temp +" ~ .(coefs_buzz$coef1) ))
abline(buzz_lin, col = "black", lwd = "3")

q10_buzz <- ((coefs_buzz$coef2 * 25)+ coefs_buzz$coef1)/((coefs_buzz$coef2 * 35)+ coefs_buzz$coef1)
mtext(bquote(paste("Q"[10]* " =  ",  .(q10_buzz))))

# quadratic model
quadratic_buzz <-lm(buzz_avg ~ temperature + temperature2, data = overall)
quadbuzz <- predict(quadratic_buzz,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadbuzz, col = "red", lwd = 3)

# logarithmic model
exp_buzz <- lm(log(buzz_avg) ~ temperature, data = overall)
exp_buzzes <- exp(predict(exp_buzz,list(temperature=tempvalues)))
lines(tempvalues, exp_buzzes, col = "blue", lwd = 3)

# (1) Scrape Rate
srate_lin <- lm(srates_avg~temperature, data = overall)

coefs_srate <- list(coef1 = round(coef(srate_lin)[1], digits = 4), coef2 = round(coef(srate_lin)[2], digits = 4))
plot(srates_avg~temperature, data = overall,  main = "Average Scrape Rates for all trials as a function of temperature", sub = bquote("scrape rate =" ~ .(coefs_srate$coef2) ~ "* temp +" ~ .(coefs_srate$coef1) ), col = "black", pch = 21, bg = "plum", ylab = "Scrape Rate (scrapes/second)", xlab = expression(paste("Temperature (", degree,"C)")))
abline(srate_lin, col = "black")
q10_srate <- ((coefs_srate$coef2 * 35)+ coefs_srate$coef1)/((coefs_srate$coef2 * 25)+ coefs_srate$coef1)
mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_srate, digits = 4)))))

#(2) Buzz Frequency
freq_lin <- lm(fundfreq_avg~temperature, data = overall)
coefs_f <-list(coef1 = round(coef(freq_lin)[1], digits = 4), coef2 = round(coef(freq_lin)[2], digits = 4))
plot(overall$fundfreq_avg~overall$temperature, col = "black", pch = 21, bg = "yellow", ylab = "Buzz Frequency (Hz)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Frequency as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_f$coef2) ~ "* temp +" ~ .(coefs_f$coef1) ))
abline(freq_lin, col = "black")

dev.off()

#writes summaries for all models to a .txt file
sink("regression_summaries.txt", append=FALSE, split=TRUE)
cat("Scrape Duration\n")
print(summary(linear_scrape))
print(summary(quadratic_scrape))
print(summary(exp_scrape))

print("Thump Duration\n")
print(summary(thump_lin))
print(summary(quadratic_thump))
print(summary(exp_thump))

cat("Buzz Duration\n")
print(summary(buzz_lin))
print(summary(quadbuzz))
print(summary(exp_buzz))

cat("Scrape Rate\n")
print(summary(srate_lin))

cat("Buzz Frequency\n")
print(summary(freq_lin))
sink()


#mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_f, digits = 4)))))




#test <- nls(scrape_avg ~  I(temperature^powerguess) - .4, data = overall, start = list(powerguess = .5), trace = T)
#expscrape <- lm(overall$scrape_avg ~ I(overall$temperature^test$coefficients[1]))
#expscrapes <-exp(predict(expscrape,list(temperature = tempvalues)))
#powerc <- round(summary(test)$coefficients[1], 3)
#power.se <- round(summary(test)$coefficients[2], 3)
#plot(scrape_avg~temperature, data = overall,  main = "Average scrape durations for all trials as a function of temperature", sub = bquote("scrape duration =" ~ .(coefs_s$coef2) ~ "* temp +" ~ .(coefs_s$coef1) ), col = "black", pch = 21, bg = "green", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")))
#lines(tempvalues, predict(test, list(x = tempvalues)), lty = 1, col = "blue")
#lines(tempvalues, (tempvalues^powerc), lty = 2, col = "green")

#lines(tempvalues, quadscrapes, col = "red", lwd = 3)

#(1) Scrape Duration

# linear model
linear_scrape <- lm(scrape_avg~temperature, data = overall)
coefs_scrape <- list( coef1 = coef(linear_scrape)[1], coef2 = coef(linear_scrape)[2])
plot(scrape_avg~temperature, data = overall,  main = "Average scrape durations for all trials as a function of temperature", sub = bquote("scrape duration =" ~ .(coefs_scrape$coef2) ~ "* temp +" ~ .(coefs_scrape$coef1) ), col = "black", pch = 21, bg = "green", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")))
abline(linear_scrape, col = "black", lwd = "3")
summary(linear_scrape)

# setting up quadratic model
tempvalues <- seq(15, 55, 0.1)
temperature2 <- overall$temperature^2
quadratic_scrape <-lm(scrape_avg ~ temperature + temperature2, data = overall)
quadscrapes <- predict(quadratic_scrape,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadscrapes, col = "red", lwd = 3)
summary(quadratic_scrape)

# setting up third order function
temperature3 <- overall$temperature^3
third_scrape <-lm(scrape_avg ~ temperature + temperature2 + temperature3, data = overall)
thirdscrapes <- predict(third_scrape,list(temperature=tempvalues, temperature2=tempvalues^2, temperature3 = tempvalues^3 ))
lines (tempvalues, thirdscrapes, col="purple", lwd = 3)
summary(third_scrape)


#setting up simple logarithmic model
exp_scrape <- lm(log(scrape_avg) ~ temperature, data = overall)
exp_scrapes <- exp(predict(exp_scrape,list(temperature=tempvalues)))
lines(tempvalues, exp_scrapes, col = "blue", lwd = 3)
summary(exp_scrape)

#(5) Thump Duration
thump_lin <- lm(thump_avg~temperature, data = overall)
coefs_thump <-list(coef1 = round(coef(thump_lin)[1], digits = 4), coef2 = round(coef(thump_lin)[2], digits = 4))
plot(overall$thump_avg~overall$temperature, col = "black", pch = 21, bg = "maroon", ylab = "Thump Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Thump Duration as a Function of Temperature", sub = bquote("thump duration =" ~ .(coefs_thump$coef2) ~ "* temp +" ~ .(coefs_thump$coef1) ))
abline(thump_lin, col = "black", lwd = "3")
q10_thump <- ((coefs_thump$coef2 * 25)+ coefs_thump$coef1)/((coefs_thump$coef2 * 35)+ coefs_thump$coef1)
mtext(bquote(paste("Q"[10]* " =  ", .(round(q10_thump, digits = 4)))))
summary(thump_lin)

# 2nd order function
quadratic_thump <-lm(thump_avg ~ temperature + temperature2, data = overall)
quadthumps <- predict(quadratic_thump,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadthumps, col = "red", lwd = 3)
summary(quadratic_thump)

# 3rd order function
third_thump <-lm(thump_avg ~ temperature + temperature2 + temperature3, data = overall)
thirdthumps <- predict(third_thump,list(temperature=tempvalues, temperature2=tempvalues^2, temperature3 = tempvalues^3 ))
lines (tempvalues, thirdthumps, col="purple", lwd = 3)
summary(third_thump)

# logarithmic model
exp_thump <- lm(log(thump_avg) ~ temperature, data = overall)
exp_thumps <- exp(predict(exp_thump,list(temperature=tempvalues)))
lines(tempvalues, exp_thumps, col = "blue", lwd = 3)
summary(exp_thump)

#(4) Buzz Duration
buzz_lin <- lm(buzz_avg~temperature, data = overall)
coefs_buzz <-list(coef1 = round(coef(buzz_lin)[1], digits = 4), coef2 = round(coef(buzz_lin)[2], digits = 4))
plot(overall$buzz_avg~overall$temperature, col = "black", pch = 21, bg = "orange", ylab = "Buzz Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Duration as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_buzz$coef2) ~ "* temp +" ~ .(coefs_buzz$coef1) ))
abline(buzz_lin, col = "black", lwd = "3")
q10_buzz <- ((coefs_buzz$coef2 * 25)+ coefs_buzz$coef1)/((coefs_buzz$coef2 * 35)+ coefs_buzz$coef1)
mtext(bquote(paste("Q"[10]* " =  ",  .(q10_buzz))))
summary(buzz_lin)

# quadratic model
quadratic_buzz <-lm(buzz_avg ~ temperature + temperature2, data = overall)
quadbuzz <- predict(quadratic_buzz,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadbuzz, col = "red", lwd = 3)
summary(quadratic_buzz)
# 3rd order function
# setting up third order function

third_buzz <-lm(buzz_avg ~ temperature + temperature2 + temperature3, data = overall)
thirdbuzzes <- predict(third_buzz,list(temperature=tempvalues, temperature2=tempvalues^2, temperature3 = tempvalues^3 ))
lines (tempvalues, thirdbuzzes, col="purple", lwd = 3)
summary(third_buzz)
# logarithmic model
exp_buzz <- lm(log(buzz_avg) ~ temperature, data = overall)
exp_buzzes <- exp(predict(exp_buzz,list(temperature=tempvalues)))
lines(tempvalues, exp_buzzes, col = "blue", lwd = 3)
summary(exp_buzz)
# (1) Scrape Rate
srate_lin <- lm(srates_avg~temperature, data = overall)

coefs_srate <- list(coef1 = round(coef(srate_lin)[1], digits = 4), coef2 = round(coef(srate_lin)[2], digits = 4))
plot(srates_avg~temperature, data = overall,  main = "Average Scrape Rates for all trials as a function of temperature", sub = bquote("scrape rate =" ~ .(coefs_srate$coef2) ~ "* temp +" ~ .(coefs_srate$coef1) ), col = "black", pch = 21, bg = "plum", ylab = "Scrape Rate (scrapes/second)", xlab = expression(paste("Temperature (", degree,"C)")))
abline(srate_lin, col = "black")
q10_srate <- ((coefs_srate$coef2 * 35)+ coefs_srate$coef1)/((coefs_srate$coef2 * 25)+ coefs_srate$coef1)
mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_srate, digits = 4)))))
summary(srate_lin)

# logarithmic model
exp_srate <- lm(log(srates_avg) ~ temperature, data = overall)
exp_srates <- exp(predict(exp_srate,list(temperature=tempvalues)))
lines(tempvalues, exp_srates, col = "blue", lwd = 3)
summary(exp_srate)

#(2) Buzz Frequency
freq_lin <- lm(fundfreq_avg~temperature, data = overall)
coefs_f <-list(coef1 = round(coef(freq_lin)[1], digits = 4), coef2 = round(coef(freq_lin)[2], digits = 4))
plot(overall$fundfreq_avg~overall$temperature, col = "black", pch = 21, bg = "yellow", ylab = "Buzz Frequency (Hz)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Frequency as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_f$coef2) ~ "* temp +" ~ .(coefs_f$coef1) ))
abline(freq_lin, col = "black")
summary(freq_lin)
# logarithmic model
exp_freq <- lm(log(fundfreq_avg) ~ temperature, data = overall)
exp_freqs <- exp(predict(exp_freq,list(temperature=tempvalues)))
lines(tempvalues, exp_freqs, col = "blue", lwd = 3)
summary(exp_freq)

# Thump RMS
trms_lin <- lm(trms_avg~temperature, data = overall)
coefs_trms <-list(coef1 = round(coef(trms_lin)[1], digits = 4), coef2 = round(coef(trms_lin)[2], digits = 4))
plot(overall$trms_avg~overall$temperature, col = "black", pch = 21, bg = "yellow", ylab = "Buzz Frequency (Hz)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Frequency as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_f$coef2) ~ "* temp +" ~ .(coefs_f$coef1) ))
abline(trms_lin, col = "black")
summary(trms_lin)
# logarithmic model
exp_trms <- lm(log(trms_avg) ~ temperature, data = overall)
exp_trms_s <- exp(predict(exp_trms,list(temperature=tempvalues)))
lines(tempvalues, exp_trms_s, col = "blue", lwd = 3)
summary(exp_trms)

# Buzz RMS
brms_lin <- lm(brms_avg~temperature, data = overall)
coefs_brms <-list(coef1 = round(coef(brms_lin)[1], digits = 4), coef2 = round(coef(brms_lin)[2], digits = 4))
plot(overall$brms_avg~overall$temperature, col = "black", pch = 21, bg = "yellow", ylab = "Buzz Frequency (Hz)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Frequency as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_f$coef2) ~ "* temp +" ~ .(coefs_f$coef1) ))
abline(brms_lin, col = "black")
summary(brms_lin)
# logarithmic model
exp_brms <- lm(log(brms_avg) ~ temperature, data = overall)
exp_brms_s <- exp(predict(exp_brms,list(temperature=tempvalues)))
lines(tempvalues, exp_brms_s, col = "blue", lwd = 3)
summary(exp_brms)


dev.off()

#writes summaries for all models to a .txt file
sink("regression_summaries.txt", append=FALSE, split=TRUE)
cat("Scrape Duration\n")
print(summary(linear_scrape))
print(summary(quadratic_scrape))
print(summary(exp_scrape))

print("Thump Duration\n")
print(summary(thump_lin))
print(summary(quadratic_thump))
print(summary(exp_thump))

cat("Buzz Duration\n")
print(summary(buzz_lin))
print(summary(quadbuzz))
print(summary(exp_buzz))

cat("Scrape Rate\n")
print(summary(srate_lin))

cat("Buzz Frequency\n")
print(summary(freq_lin))
sink()


#mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_f, digits = 4)))))




#test <- nls(scrape_avg ~  I(temperature^powerguess) - .4, data = overall, start = list(powerguess = .5), trace = T)
#expscrape <- lm(overall$scrape_avg ~ I(overall$temperature^test$coefficients[1]))
#expscrapes <-exp(predict(expscrape,list(temperature = tempvalues)))
#powerc <- round(summary(test)$coefficients[1], 3)
#power.se <- round(summary(test)$coefficients[2], 3)
#plot(scrape_avg~temperature, data = overall,  main = "Average scrape durations for all trials as a function of temperature", sub = bquote("scrape duration =" ~ .(coefs_s$coef2) ~ "* temp +" ~ .(coefs_s$coef1) ), col = "black", pch = 21, bg = "green", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")))
#lines(tempvalues, predict(test, list(x = tempvalues)), lty = 1, col = "blue")
#lines(tempvalues, (tempvalues^powerc), lty = 2, col = "green")

#lines(tempvalues, quadscrapes, col = "red", lwd = 3)

# mixed model
summary(lme(scrape_avg ~ treatment, data = complete, random = ~ 1 | individual))

summary(lme(thump_avg ~ treatment, data = complete, random = ~ 1 | individual))

summary(lme(buzz_avg ~ treatment, data = complete, random = ~ 1 | individual))
summary(lme(fundfreq_avg ~ treatment, data = complete, random = ~ 1 | individual))


uniques <- unique(complete$individual)
scrape_avg <- c(complete$individual, complete$scrape_avg, complete$weight, complete$ct_width)

scrape_avg <-as.matrix(complete[c("individual","scrape_avg","weight", "ct_width")])