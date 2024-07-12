####Set up####
##read in the data
read = read.csv("Data/Control Group.csv")
iJOL = read.csv("Data/JOL Item.csv")
gJOL = read.csv("Data/JOL Global.csv")

length(unique(read$Username))
length(unique(iJOL$Username))
length(unique(gJOL$Username))

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
              read$Username != "20291142" & read$Username != "M20303251_AAR" & read$Username != "M20304460_DNI" &
              read$Username != "M20322322") #recog @ 100%

#global
#none so far

#item
iJOL = subset(iJOL,
              iJOL$Username != "M20265302_MJL") #recog @ 100%

iJOL = subset(iJOL,
              iJOL$Username != "M20331476") #too many false alarms

##Put everything together
length(unique(read$Username))
length(unique(iJOL$Username))
length(unique(gJOL$Username))

dat = rbind(read, iJOL, gJOL)

length(unique(dat$Username))

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

####okay, add in version####
vs = read.csv("ex1b version.csv")

dat.v = merge(presented, vs, by.x = "Username", by.y = "Username")

colnames(dat.v)[6] = "version"

model.v = ezANOVA(dat.v,
                 dv = scored,
                 within = Direction,
                 between = .(Encoding, version),
                 wid = Username,
                 type = 3,
                 detailed = T)

model.v

model.v$ANOVA$MSE = model.v$ANOVA$SSd/model.v$ANOVA$DFd
model.v$ANOVA$MSE

aovEffectSize(model.v, effectSize = "pes")

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

tapply(control$scored, control$Encoding, mean)

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

#get values for d
mean(encoding.ph$Item_JOL, na.rm = T); mean(encoding.ph$Global_JOL, na.rm = T)
sd(encoding.ph$Item_JOL, na.rm = T); sd(encoding.ph$Global_JOL, na.rm = T)

#IJOL VS READ
temp = t.test(encoding.ph$Read, encoding.ph$Item_JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#GJOL VS READ
temp = t.test(encoding.ph$Read, encoding.ph$Global_JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-SIG

sd(encoding.ph$Read, na.rm = T); sd(encoding.ph$Global_JOL, na.rm = T)

##get pbic
pbic1 = encoding.ph[ , c(1, 2)]
pbic2 = encoding.ph[ , c(1, 4)]

pbic1$encoding = rep("global")
pbic2$encoding = rep("read")

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = score,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T)

####Interaction####
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

sd(gjol2$Unrelated); sd(read2$Unrelated)

##pbic
pbic1 = gjol2[ , c(1, 3)]
pbic2 = read2[ , c(1, 3)]

pbic1$encoding = rep("global")
pbic2$encoding = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Username,
        dv = Unrelated,
        between = encoding,
        type = 3,
        detailed = T)

####control items####
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
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON-SIG

sd(gjol4$Control); sd(ijol4$Control)

#item vs. read
temp = t.test(ijol4$Control, read4$Control, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

mean(ijol4$Control); mean(read4$Control)
sd(ijol4$Control); sd(read4$Control)

#global vs read
temp = t.test(gjol4$Control, read4$Control, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #marginal

sd(gjol4$Control); sd(read4$Control)

##get pbics
gjol4$encoding = rep("global")
read4$encoding = rep("read")
ijol4$encoding = rep("item")

pbic1 = rbind(gjol4, read4)

ezANOVA(pbic1,
        wid = Username,
        between = encoding,
        dv = Control,
        type = 3,
        detailed = T)

pbic2 = rbind(gjol4, ijol4)

ezANOVA(pbic2,
        wid = Username,
        between = encoding,
        dv = Control,
        type = 3,
        detailed = T)

###Notes so far:
##Making Item-level JOLs improves memory for related and unrelated items (replicates under review findings w/ word pairs)
##global JOLs are not reactive
##but both types of JOLs make participants less susceptible to false recognition of control items (bigger effect w/ item based JOLs)

##get values for Table A3
(apply(ijol2, 2, sd) / sqrt(nrow(ijol2))) * 1.96
(apply(gjol2, 2, sd) / sqrt(nrow(gjol2))) * 1.96
(apply(read2, 2, sd) / sqrt(nrow(read2))) * 1.96

#test for sig here
temp = t.test(ijol2$Related, ijol2$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#get CIs for new items in Figure 2
(sd(ijol4$Control) / sqrt(nrow(ijol4))) * 1.96
(sd(gjol4$Control) / sqrt(nrow(gjol4))) * 1.96
(sd(read4$Control) / sqrt(nrow(read4))) * 1.96

####JOLs####
JOLs = read.csv("ex1b JOLs.csv")

##remove outliers
#item
JOLs = subset(JOLs,
              JOLs$Username != "M20265302_MJL")
JOLs = subset(JOLs,
              JOLs$Username != "M20331476")

tapply(JOLs$Related, JOLs$encoding, mean)
tapply(JOLs$Unrelated, JOLs$encoding, mean)

JOL.I = subset(JOLs, JOLs$encoding == "item")
JOL.G = subset(JOLs, JOLs$encoding == "global")

(apply(JOL.I[ , c(2:3)], 2, sd) / sqrt(nrow(JOL.I))) * 1.96
(apply(JOL.G[ , c(2:3)], 2, sd) / sqrt(nrow(JOL.G))) * 1.96

t.test(JOL.I$Related, JOL.I$Unrelated, paired = T, var.equal = T)
t.test(JOL.G$Related, JOL.G$Unrelated, paired = T, var.equal = T)

####Encoding Latencies####
e = read.csv("Ex1B Encoding.csv")

##remove outliers
#read
e = subset(e,
           e$Username != "20291142" & e$Username != "M20303251_AAR" & e$Username != "M20304460_DNI" &
           e$Username != "M20322322")
e = subset(e,
           e$Username != "M20265302_MJL")
e = subset(e,
           e$Username != "M20331476")

####Run the Anova####
library(ez)
library(reshape)
options(scipen = 999)

e.long = melt(e,
              measure.vars = c("Related", "Unrelated"))
colnames(e.long)[3:4] = c("List_Type", "Score")

e.long$Score[e.long$Score > 10000] = NA
e.long$Score[e.long$Score < 1000] = NA

e.long = na.omit(e.long)

ezANOVA(e.long,
        wid = Username,
        dv = Score,
        between = .(List_Type, Encoding),
        type = 3,
        detailed = T)

tapply(e.long$Score, list(e.long$Encoding, e.long$List_Type), mean)

e$experiment = rep("1B")

#write.csv(e, file = "ex1b encoding final.csv", row.names = F)
