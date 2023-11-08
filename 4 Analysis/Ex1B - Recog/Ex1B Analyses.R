####Set up####
##read in the data
read = read.csv("Data/Control Group.csv")
iJOL = read.csv("Data/JOL Item.csv")
gJOL = read.csv("Data/JOL Global.csv")

##combine
read = read[ , -c(3:4)]
iJOL = iJOL[ , -c(3:4)]
gJOL = gJOL[ , -c(3:4)]

dat = rbind(read, iJOL, gJOL)

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

#no one abysmally low (< %5; some at 100% but not for all types)

####Get descriptives####
##presented 
presented = subset(dat, dat$Direction != "Control")

tapply(presented$scored, list(presented$Encoding, presented$Direction), mean)

##false recognition of control items
control = subset(dat, dat$Direction == "Control")

#invert
control$scored = (control$scored - 1) * -1

#false alarms
tapply(control$scored, control$Encoding, mean) #lowest for items, highest for Read

####Run the ANOVA####
##presented
ezANOVA(presented,
        dv = scored,
        within = Direction,
        between = Encoding,
        wid = Username,
        type = 3,
        detailed = T)

#control items
ezANOVA(control,
        dv = scored,
        between = Encoding,
        wid = Username,
        type = 3,
        detailed = T)

##All effects significant

####Run the post-hocs####
###Start w/ presented items
tapply(presented$scored, presented$Encoding, mean) #main effect encoding group
tapply(presented$scored, presented$Direction, mean) #main effect direction
tapply(presented$scored, list(presented$Encoding, presented$Direction), mean) #interaction

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

#item vs. read
temp = t.test(ijol2$Related, read2$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

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
