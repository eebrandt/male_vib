setwd("/home/eebrandt/projects/temp_trials/male_only/analysis/")
scrape <- read.csv("output.csv", header = T)
#adds "time" column that just lists sequential numbers from 0.  This can be divided by 48000
# to get time in seconds
scrape$time <- seq(1, nrow(scrape), 1)
# rearrange columns to get everything in the proper order
scrape <- scrape[,c(2,1)]
#label the columns something meaningful
colnames(scrape) <- c("time", "voltage")

#pull out voltage and time as their own lists
voltage <- scrape$voltage
time <- scrape$time

#subsets voltage and time by taking only every other element.  Recommended for scrape and thumps
# change number to 15 or 20 for buzzes, depending on how long your buzz is.  Will choke graphics
#program otherwise
voltage <- voltage[seq(1, length(voltage), 300)]
time <- time[seq(1, length(time), 300)]

#plots voltage versus time, which can then be exported to graphics program
plot(voltage~time, col = "blue", lwd = 1, type = "l")
