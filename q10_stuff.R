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

pdf(file = "moar_plotzes!")
powerguess <- c(.5, 1, 2, 2.5)
# (1) Scrape Rate
srate_lin <- lm(srates_avg~temperature, data = overall)
coefs_srate <- list(coef1 = round(coef(srate_lin)[1], digits = 4), coef2 = round(coef(srate_lin)[2], digits = 4))
plot(srates_avg~temperature, data = overall,  main = "Average scrape rates for all trials as a function of temperature", sub = bquote("scrape rate =" ~ .(coefs_srate$coef2) ~ "* temp +" ~ .(coefs_srate$coef1) ), col = "black", pch = 21, bg = "plum", ylab = "Scrape Rate (scrapes/second)", xlab = expression(paste("Temperature (", degree,"C)")))
abline(srate_lin, col = "black")
q10_srate <- ((coefs_srate$coef2 * 35)+ coefs_srate$coef1)/((coefs_srate$coef2 * 25)+ coefs_srate$coef1)
mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_srate, digits = 4)))))

#(2) Buzz Frequency
Q10calc_freq <- lm(fundfreq_avg~temperature, data = overall)
coefs_f <-list(coef1 = round(coef(Q10calc_freq)[1], digits = 4), coef2 = round(coef(Q10calc_freq)[2], digits = 4))
plot(overall$fundfreq_avg~overall$temperature, col = "black", pch = 21, bg = "yellow", ylab = "Buzz Frequency (Hz)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Duration as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_f$coef2) ~ "* temp +" ~ .(coefs_f$coef1) ))
abline(Q10calc_freq, col = "black")
summary(Q10calc_freq)
#mtext(bquote(paste("Q"[10]* " =  " , .(round(q10_f, digits = 4)))))

#(3) Scrape Duration

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
summary(quadratic_scrape)

#setting up simple logarithmic model
exp_scrape <- lm(log(scrape_avg) ~ temperature, data = overall)
exp_scrapes <- exp(predict(exp_scrape,list(temperature=tempvalues)))
lines(tempvalues, exp_scrapes, col = "blue", lwd = 3)
summary(exp_scrape)

test <- nls(scrape_avg ~  I(temperature^powerguess) - .4, data = overall, start = list(powerguess = .5), trace = T)
#expscrape <- lm(overall$scrape_avg ~ I(overall$temperature^test$coefficients[1]))
#expscrapes <-exp(predict(expscrape,list(temperature = tempvalues)))
#powerc <- round(summary(test)$coefficients[1], 3)
#power.se <- round(summary(test)$coefficients[2], 3)
#plot(scrape_avg~temperature, data = overall,  main = "Average scrape durations for all trials as a function of temperature", sub = bquote("scrape duration =" ~ .(coefs_s$coef2) ~ "* temp +" ~ .(coefs_s$coef1) ), col = "black", pch = 21, bg = "green", ylab = "Scrape Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")))
#lines(tempvalues, predict(test, list(x = tempvalues)), lty = 1, col = "blue")
#lines(tempvalues, (tempvalues^powerc), lty = 2, col = "green")

#lines(tempvalues, quadscrapes, col = "red", lwd = 3)

#(4) Buzz Duration

buzz_lin <- lm(buzz_avg~temperature, data = overall)
coefs_buzz <-list(coef1 = round(coef(buzz_lin)[1], digits = 4), coef2 = round(coef(buzz_lin)[2], digits = 4))
plot(overall$buzz_avg~overall$temperature, col = "black", pch = 21, bg = "orange", ylab = "Buzz Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Buzz Duration as a Function of Temperature", sub = bquote("peak buzz frequency =" ~ .(coefs_buzz$coef2) ~ "* temp +" ~ .(coefs_buzz$coef1) ))
abline(buzz_lin, col = "black", lwd = "3")
summary(buzz_lin)
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
summary(exp_buzz)

#(5) Thump Duration

thump_lin <- lm(thump_avg~temperature, data = overall)
coefs_thump <-list(coef1 = round(coef(thump_lin)[1], digits = 4), coef2 = round(coef(thump_lin)[2], digits = 4))
plot(overall$thump_avg~overall$temperature, col = "black", pch = 21, bg = "maroon", ylab = "Thump Duration (seconds)", xlab = expression(paste("Temperature (", degree,"C)")), main = "Average Thump Duration as a Function of Temperature", sub = bquote("thump duration =" ~ .(coefs_thump$coef2) ~ "* temp +" ~ .(coefs_thump$coef1) ))
abline(thump_lin, col = "black", lwd = "3")
summary(thump_lin)
q10_thump <- ((coefs_thump$coef2 * 25)+ coefs_thump$coef1)/((coefs_thump$coef2 * 35)+ coefs_thump$coef1)
mtext(bquote(paste("Q"[10]* " =  ", .(round(q10_thump, digits = 4)))))

quadratic_thump <-lm(thump_avg ~ temperature + temperature2, data = overall)
quadthumps <- predict(quadratic_scrape,list(temperature=tempvalues, temperature2=tempvalues^2))
lines(tempvalues, quadthumps, col = "red", lwd = 3)
summary(quadratic_thump)

# logarithmic model
exp_thump <- lm(log(thump_avg) ~ temperature, data = overall)
exp_thumps <- exp(predict(exp_thump,list(temperature=tempvalues)))
lines(tempvalues, exp_thumps, col = "blue", lwd = 3)
summary(exp_thump)
dev.off()