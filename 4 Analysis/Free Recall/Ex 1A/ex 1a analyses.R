####Set up####
##read in data
dat = read.csv("Data/ex1a.csv")

##load libraries
library(reshape)
library(ez)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

##fix column names
colnames(dat)[1:2] = c("ID", "List_Type")
colnames(dat)[4] = "scored"

#get ns
global = subset(dat,
                dat$encoding == "global")

length(unique(global$ID)) #38 - 1
table(global$ID)

item = subset(dat,
              dat$encoding == "item")
table(item$ID)

#remove participant who didn't complete experiment
dat = subset(dat,
             dat$ID != "653b9ed2c4b3466a16bc6e05")

length(unique(item$ID)) #37 - 1

read = subset(dat,
              dat$encoding == "read")
table(read$ID)

length(unique(read$ID)) #39
##any more cheaters/slackers?
global.wide = cast(global, ID ~ List_Type, mean)
item.wide = cast(item, ID ~ List_Type, mean)
read.wide = cast(read, ID ~ List_Type, mean) #Nope, finally looks good!

####Descriptives####
tapply(dat$scored, dat$encoding, mean) 

tapply(dat$scored, list(dat$encoding, dat$List_Type), mean)

model1 = ezANOVA(dat,
        wid = ID,
        within = List_Type,
        between = encoding,
        dv = scored,
        type = 3,
        detailed = T)
model1

#get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

##marginal main effect encoding
##signicant effect list type
##significant interaction
tapply(dat$scored, dat$encoding, mean) #encoding
tapply(dat$scored, dat$List_Type, mean) #list type
tapply(dat$scored, list(dat$encoding, dat$List_Type), mean) #interaction

####Post-hocs####
dat.ph = cast(dat, ID ~ encoding, mean)

#break down marginal main effect
temp = t.test(dat.ph$global, dat.ph$item, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

mean(dat.ph$global, na.rm = T); mean(dat.ph$item, na.rm = T)
sd(dat.ph$global, na.rm = T); sd(dat.ph$item, na.rm = T)

temp = t.test(dat.ph$global, dat.ph$read, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

##get pbic
pbic1 = dat.ph[ , c(1,2)]
pbic2 = dat.ph[ , c(1,4)]

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

pbic1$encoding = rep("global")
pbic2$encoding = rep("read")

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                 wid = ID,
                 between = encoding,
                 dv = score,
                 type = 3,
                 detailed = T)
model

temp = t.test(dat.ph$item, dat.ph$read, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

####Interaction####
##related lists
temp = t.test(global.wide$Related, item.wide$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

##get pbics
pbic1 = global.wide[ , c(1:2)]
pbic2 = item.wide[ , c(1:2)]

pbic1$encoding = rep("global")
pbic2$encoding = rep("item")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = ID,
                between = encoding,
                dv = Related,
                type = 3,
                detailed = T)
model

temp = t.test(global.wide$Related, read.wide$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #sig

#get means/sd for d
mean(global.wide$Related); mean(read.wide$Related)
sd(global.wide$Related); sd(read.wide$Related)

temp = t.test(item.wide$Related, read.wide$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

##making global JOLs boosts free-recall of related lists

##unrelated
temp = t.test(global.wide$Unrelated, item.wide$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #global boosted vs item

#get means/sds
mean(global.wide$Unrelated); mean(item.wide$Unrelated)
sd(global.wide$Unrelated); sd(item.wide$Unrelated)

temp = t.test(global.wide$Unrelated, read.wide$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

pbic1 = global.wide[ , c(1, 3)]
pbic2 = read.wide[ , c(1, 3)]

pbic1$encoding = rep("global")
pbic2$encoding = rep("read")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = ID,
                between = encoding,
                dv = Unrelated,
                type = 3,
                detailed = T)
model

temp = t.test(item.wide$Unrelated, read.wide$Unrelated, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

##get 95% CI values for Table A1
(apply(item.wide, 2, sd) / sqrt(nrow(item.wide))) * 1.96
(apply(global.wide, 2, sd) / sqrt(nrow(global.wide))) * 1.96
(apply(read.wide, 2, sd) / sqrt(nrow(read.wide))) * 1.96

