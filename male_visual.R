#male visual display data

setwd("/home/eebrandt/projects/dissertation/chapter_1/female_choice/data/processed_times")
visdisp <- read.csv("female_time_final.csv")
setwd("../../../female_choice/analysis/")

visdisp$weight.ratio <- (visdisp$male.weight/visdisp$female.weight)
visdisp$escapes.trial <- (visdisp$number.of.female.escapes/(visdisp$trial.length/60))

hot <- subset(visdisp, treatment == "warm")
cold <- subset(visdisp, treatment == "cold")

accept <- subset(hot, outcome =="accept")
reject <- subset(visdisp, outcome == "reject")

# male
par(mfrow=c(1,1))
plot(visdisp$weight.ratio~visdisp$outcome, col = c("white", "grey"), ylab = "male:female weight ratio", xlab = "treatment", main = "weight ratio and outcome")
par(mfrow=c(3,2))
plot(visdisp$number.of.sidles~visdisp$treatment, xlab = "treatment", ylab = "number of sidles", col = c("light blue", "pink"), main = "number of sidles")
plot(visdisp$length.of.sidling.bout~visdisp$treatment, xlab = "treatment", ylab = "length of sidle", col = c("light blue", "pink"), main = "average length of sidle")
plot(visdisp$percent.of.trial.spent.sidling~visdisp$treatment, xlab = "treatment", ylab = "percent of trial spent sidling", col = c("light blue", "pink"), main = "percent of trial spent sidling")
plot(visdisp$number.of.movement.bouts~visdisp$treatment, ylab = "number of movement bouts", xlab = "treatment", col = c("light blue", "pink"), main =  "number of movement bouts")
plot(visdisp$length.of.movement.bout~visdisp$treatment, ylab = "length of movement bouts", xlab = "treatment", col = c("light blue", "pink"), main = "length of movement bouts")
plot(visdisp$percent.of.movement.time.per.sidle~visdisp$treatment, ylab = "percent of sidle spent moving", xlab = "treatment", col = c("light blue", "pink"), main = "percent of sidle spent moving")

# female
par(mfrow=c(1,3))
plot(visdisp$number.of.female.attack.jumps~visdisp$treatment, ylab = "female attacks", xlab = "treatment", col = c("light blue", "pink"), main = "number of female attacks")
plot(visdisp$grappling.present~visdisp$treatment, ylab = "female grapples", xlab = "treatment", col = c("light blue", "pink"), main = "number of female grapples")
plot(visdisp$escapes.trial~visdisp$treatment, ylab = "female escapes/minute", xlab = "treatment", col = c("light blue", "pink"), main = "female escapes per minute")

#weight stuff

# male

plot(hot$number.of.sidles~hot$weight.ratio, col = "red", xlab = "weight ratio", ylab = "number of sidles")
lines(cold$number.of.sidles~cold$weight.ratio, col = "blue", type = "p")

plot(hot$time.spent.sidling~hot$weight.ratio, col = "red", xlab = "weight ratio", ylab = "time spent sidling")
lines(cold$time.spent.sidling~cold$weight.ratio, col = "blue", type = "p")

#female
plot(hot$grappling.present~hot$weight.ratio, col = "red", xlab = "weight ratio", ylab = "grappling present")
lines(cold$grappling.present~cold$weight.ratio, col = "blue", type = "p")

plot(hot$number.of.female.attack.jumps~hot$weight.ratio, col = "red", xlab = "weight ratio", ylab = "number of attack jumps")
lines(cold$number.of.female.attack.jumps~cold$weight.ratio, col = "blue", type = "p")

plot(hot$escapes~hot$weight.ratio, col = "red", xlab = "weight ratio", ylab = "number of attack jumps")
lines(cold$number.of.female.escapes~cold$weight.ratio, col = "blue", type = "p")

# just warm outcome stuff
par(mfrow=c(3,2))
plot(hot$number.of.sidles~hot$outcome, xlab = "outcome", ylab = "number of sidles", col = c("white", "grey"), main = "number of sidles")
plot(hot$length.of.sidling.bout~hot$outcome, xlab = "outcome", ylab = "length of sidle", col = c("white", "grey"), main = "average length of sidle")
plot(hot$percent.of.trial.spent.sidling~hot$outcome, xlab = "outcome", ylab = "percent of trial spent sidling", col = c("white", "grey"), main = "percent of trial spent sidling")
plot(hot$number.of.movement.bouts~hot$outcome, ylab = "number of movement bouts", xlab = "outcome", col = c("white", "grey"), main =  "number of movement bouts")
plot(hot$length.of.movement.bout~hot$outcome, ylab = "length of movement bouts", xlab = "outcome", col = c("white", "grey"), main = "length of movement bouts")
plot(hot$percent.of.movement.time.per.sidle~hot$outcome, ylab = "percent of sidle spent moving", xlab = "outcome", col = c("white", "grey"), main = "percent of sidle spent moving")

par(mfrow=c(1,3))
plot(hot$number.of.female.attack.jumps~hot$outcome, ylab = "female attacks", xlab = "treatment", col = c("white", "grey"), main = "number of female attacks")
plot(hot$grappling.present~hot$outcome, ylab = "female grapples", xlab = "treatment", col = c("white", "grey"), main = "number of female grapples")
plot(hot$escapes.trial~hot$outcome, ylab = "female escapes/minute", xlab = "treatment", col = c("white", "grey"), main = "female escapes per minute")

# stats

sidle_t = t.test(hot$number.of.sidles, cold$number.of.sidles)
per_sidle_t = t.test(hot$time.spent.sidling, cold$time.spent.sidling)
movlength_t = t.test(hot$length.of.movement.bout, cold$length.of.movement.bout)

escape_t = t.test(hot$number.of.female.escapes, cold$number.of.female.escapes)

sidleacc_t = t.test(accept$number.of.sidles, reject$number.of.sidles)
per_sidleacc_t = t.test(accept$time.spent.sidling, reject$time.spent.sidling)
movlengthacc_t = t.test(accept$length.of.movement.bout, reject$length.of.movement.bout)

pdf("visual_plots.pdf")
par(mfrow=c(1,3))
plot(visdisp$number.of.sidles~visdisp$treatment, xlab = "treatment", ylab = "number of sidles", col = c("blue", "red"), main = "number of sidles")
plot(visdisp$percent.of.trial.spent.sidling~visdisp$treatment, xlab = "treatment", ylab = "percent of trial spent sidling", col = c("blue", "red"), main = "percent of trial spent sidling")
plot(visdisp$length.of.movement.bout~visdisp$treatment, ylab = "length of movement bouts", xlab = "treatment", col = c("blue", "red"), main = "length of movement bouts")
dev.off()

# round 1 only 
visdisprd1 <- subset(visdisp, round.number == "1")
visdisprd1_h <- subset(visdisprd1, treatment == "warm")
visdisprd1_c <- subset(visdisprd1, treatment == "cold")



par(mfrow=c(1,3))
plot(visdisprd1$number.of.sidles~visdisprd1$treatment, xlab = "treatment", ylab = "number of sidles", col = c(rgb(93, 93, 255, maxColorValue = 255), rgb(255, 92, 92, maxColorValue = 255)), main = "number of sidles")
plot(visdisprd1$percent.of.trial.spent.sidling~visdisprd1$treatment, xlab = "treatment", ylab = "percent of trial spent sidling", col = c(rgb(93, 93, 255, maxColorValue = 255), rgb(255, 92, 92, maxColorValue = 255)), main = "percent of trial spent sidling")
plot(visdisprd1$length.of.movement.bout~visdisprd1$treatment, ylab = "length of movement bouts", xlab = "treatment", col = c(rgb(93, 93, 255, maxColorValue = 255), rgb(255, 92, 92, maxColorValue = 255)), main = "length of movement bouts")

# direct output to a file 
sink("visual_stats.txt", append=TRUE, split=FALSE)
t.test(visdisprd1_h$number.of.sidles, visdisprd1_c$number.of.sidles)
sd(visdisprd1_h$number.of.sidles)
sd(visdisprd1_c$number.of.sidles)

t.test(visdisprd1_h$percent.of.trial.spent.sidling, visdisprd1_c$percent.of.trial.spent.sidling)
sd(visdisprd1_h$percent.of.trial.spent.sidling)
sd(visdisprd1_c$percent.of.trial.spent.sidling)

t.test(visdisprd1_h$length.of.movement.bout, visdisprd1_c$length.of.movement.bout)
sd(visdisprd1_h$length.of.movement.bout)
sd(visdisprd1_c$length.of.movement.bout)
sink()

# round 2 only
visdisprd2 <- subset(visdisp, round.number == "2")
visdisprd2_h <- subset(visdisprd2, treatment == "warm")
visdisprd2_c <- subset(visdisprd2, treatment == "cold")

par(mfrow=c(1,3))
plot(visdisprd2$number.of.sidles~visdisprd2$treatment, xlab = "treatment", ylab = "number of sidles", col = c("blue", "red"), main = "number of sidles")
plot(visdisprd2$percent.of.trial.spent.sidling~visdisprd2$treatment, xlab = "treatment", ylab = "percent of trial spent sidling", col = c("blue", "red"), main = "percent of trial spent sidling")
plot(visdisprd2$length.of.movement.bout~visdisprd2$treatment, ylab = "length of movement bouts", xlab = "treatment", col = c("blue", "red"), main = "length of movement bouts")

t.test(visdisprd2_h$number.of.sidles, visdisprd2_c$number.of.sidles)
t.test(visdisprd2_h$percent.of.trial.spent.sidling, visdisprd2_c$percent.of.trial.spent.sidling)
t.test(visdisprd2_h$length.of.movement.bout, visdisprd2_c$length.of.movement.bout)

