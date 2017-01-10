
library("lattice", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("nlme", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("lme4", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")
library("gvlma", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.1")

setwd("~/projects/temp_trials/male_only/data")
details <- file.info(list.files(pattern="temp_vibration_data*"))
details <- details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
loadfile <- tail(files,1)
overall <-read.csv(loadfile)
complete <-subset(overall, complete == TRUE)
setwd("~/projects/temp_trials/male_only/analysis/")

as.factor(overall$individual)

# OK, so we need to do some basic data exploration.  Let's work with one aspect at a time.
# first just make a standard lm of the data
M.lm <- lm(scrape_avg~temperature, data = overall)
plot(M.lm, select = c(1))

log_savg <- log(overall$scrape_avg + 1)
M2.lm <- lm(log_savg~temperature, data = overall)
E <- rstandard(M2.lm)

boxplot(E~individual, data = overall)
abline(0,0); axis(2)

Form1<- formula(scrape_avg~temperature, data = overall)

Form <- formula(log_savg ~ temperature)
M.gls <- gls(Form1, data = overall)

M1.lme <- lme(Form1, random = ~1|individual, method = "REML", data = overall)
anova(M.gls, M1.lme)

E2 <- resid(M1.lme, type= "normalized")
F2 <- fitted(M1.lme)
plot(x = F2, y = E2)