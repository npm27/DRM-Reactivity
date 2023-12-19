####Set up####
##read in the data
read = read.csv("Data/Control Group.csv")
iJOL = read.csv("Data/JOL Item.csv")
gJOL = read.csv("Data/JOL Global.csv")

##combine
read = read[ , -c(3:4)]
iJOL = iJOL[ , -c(3:4)]
gJOL = gJOL[ , -c(3:4)]

##load libraries#
library(reshape)
library(ez)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

####Check the data####
gJOL.wide = cast(gJOL[ , c(1, 3, 5)], Username ~ Direction, mean)
iJOL.wide = cast(iJOL[ , c(1, 3, 5)], Username ~ Direction, mean)
read.wide = cast(read[ , c(1, 3, 5)], Username ~ Direction, mean)

#read
read = subset(read,
              read$Username != "20291142" | read$Username != "M20303251_AAR" | read$Username != "M20304460_DNI" |
              read$Username != "M20322322") #recog @ 100%

#global
#none so far

#item
iJOL = subset(iJOL,
              iJOL$Username != "M20265302_MJL") #recog @ 100%

iJOL = subset(iJOL,
              iJOL$Username != "M20331476") #too many false alarms

##Put everything together
dat = rbind(read, iJOL, gJOL)

##invert control items
presented = subset(dat,
                   dat$Direction != "Control")

control = subset(dat, 
                 dat$Direction == "Control")

control$scored = (control$scored * -1) + 1

dat = rbind(presented, control)

##write to file for signal detection
#write.csv(dat, file = "sd ex1B.csv", row.names = F)

####Get descriptives####
##presented 
tapply(presented$scored, list(presented$Encoding, presented$Direction), mean)

##false recognition of control items
control = subset(dat, dat$Direction == "Control")

#false alarms
tapply(control$scored, control$Encoding, mean) #lowest for items, highest for Read, probably not sig though

####Run the ANOVA####
##presented
model1 = ezANOVA(presented,
        dv = scored,
        within = Direction,
        between = Encoding,
        wid = Username,
        type = 3,
        detailed = T)

model1

#get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

#control items
model2 = ezANOVA(control,
        dv = scored,
        between = Encoding,
        wid = Username,
        type = 3,
        detailed = T)

model2

#get MSE here
model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

####Run the post-hocs####
###Start w/ presented items
tapply(presented$scored, presented$Encoding, mean) #main effect encoding group
tapply(presented$scored, presented$Direction, mean) #main effect direction
tapply(presented$scored, list(presented$Encoding, presented$Direction), mean) #interaction

tapply(control$scored, control$Encoding, mean)

##Main effect of encoding group
encoding.ph = cast(presented, Username ~ Encoding, mean)

#IJOL vs GJOL
temp = t.test(encoding.ph$Global_JOL, encoding.ph$Item_JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#IJOL VS READ
temp = t.test(encoding.ph$Read, encoding.ph$Item_JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#get values for d
mean(encoding.ph$Item_JOL, na.rm = T); mean(encoding.ph$Read, na.rm = T)
sd(encoding.ph$Item_JOL, na.rm = T); sd(encoding.ph$Read, na.rm = T)

#GJOL VS READ
temp = t.test(encoding.ph$Read, encoding.ph$Global_JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

##break down the interaction
gjol = subset(presented, presented$Encoding == "Global_JOL")
ijol = subset(presented, presented$Encoding == "Item_JOL")
read = subset(presented, presented$Encoding == "Read")

gjol2 = cast(gjol, Username ~ Direction, mean)
ijol2 = cast(ijol, Username ~ Direction, mean)
read2 = cast(read, Username ~ Direction, mean)

###Related Lists
#global vs item
temp = t.test(gjol2$Related, ijol2$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

mean(ijol2$Related); mean(gjol2$Related)
sd(ijol2$Related); sd(gjol2$Related)

#item vs. read
temp = t.test(ijol2$Related, read2$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

mean(ijol2$Related); mean(read2$Related)
sd(ijol2$Related); sd(read2$Related)

#global vs read
temp = t.test(gjol2$Related, read2$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##Unrelated
#global vs item
temp = t.test(gjol2$Unrelated, ijol2$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#item vs. read
temp = t.test(ijol2$Unrelated, read2$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#global vs read
temp = t.test(gjol2$Unrelated, read2$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

###Unrelated
gjol3 = subset(control, control$Encoding == "Global_JOL")
ijol3 = subset(control, control$Encoding == "Item_JOL")
read3 = subset(control, control$Encoding == "Read")

gjol4 = cast(gjol3, Username ~ Direction, mean)
ijol4 = cast(ijol3, Username ~ Direction, mean)
read4 = cast(read3, Username ~ Direction, mean)

##t-tests
#global vs item
temp = t.test(gjol4$Control, ijol4$Control, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#item vs. read
temp = t.test(ijol4$Control, read4$Control, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#global vs read
temp = t.test(gjol4$Control, read4$Control, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

###Notes so far:
##Making Item-level JOLs improves memory for related and unrelated items (replicates under review findings w/ word pairs)
##global JOLs are not reactive
##but both types of JOLs make participants less susceptible to false recognition of control items (bigger effect w/ item based JOLs)
