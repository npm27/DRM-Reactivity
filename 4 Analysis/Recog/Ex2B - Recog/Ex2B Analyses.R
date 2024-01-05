####Set up####
##read in the data
read = read.csv("Data/Control Group.csv")
iJOL = read.csv("Data/JOL Item.csv")
gJOL = read.csv("Data/JOL Global.csv")

colnames(read)[2] = "Item_type"
colnames(iJOL)[2] = "Item_type"
colnames(gJOL)[2] = "Item_type"

##load libraries#
library(reshape)
library(ez)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

####Check the data####
gJOL.wide = cast(gJOL, Username ~ Item_type, mean)
iJOL.wide = cast(iJOL, Username ~ Item_type, mean)
read.wide = cast(read, Username ~ Item_type, mean)

#read
read = subset(read,
              read$Username != "KamiyaSlaughter") #super high on non-presented items

#global
#none so far
gJOL = subset(gJOL,
              gJOL$Username != "ValeriaMunoz") #at chance for everything -- suggests guessing

#item
iJOL = subset(iJOL,
              iJOL$Username != "HayleyGardner") #Poor performance
iJOL = subset(iJOL,
              iJOL$Username != "KezaUwamahoro") #marked "old" for every item...

dat = rbind(read, iJOL, gJOL)

##Write to file for signal detection
#write.csv(dat, file = "sd ex2b.csv", row.names = F)

####Get descriptives####
##presented 
presented = subset(dat, dat$Item_type == "List Item")

tapply(presented$scored, presented$encoding, mean)

##false recognition of critical lures
CL = subset(dat, dat$Item_type == "Critical Lure")

tapply(CL$scored, CL$encoding, mean)

##control
control = subset(dat,
                 dat$Item_type != "List Item" & dat$Item_type != "Critical Lure")

####Run the ANOVA####
##presented
model1 = ezANOVA(presented,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #higher correct recognition for item JOLs

#get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

##post-hocs
#set up the data
presented.ph = cast(presented, Username ~ encoding, mean)

#global vs item
temp = t.test(presented.ph$global, presented.ph$item, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#get d's
mean(presented.ph$global, na.rm = T); mean(presented.ph$item, na.rm = T)
sd(presented.ph$global, na.rm = T); sd(presented.ph$item, na.rm = T)

#item vs read
temp = t.test(presented.ph$read, presented.ph$item, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#global vs read
temp = t.test(presented.ph$read, presented.ph$global, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON SIG

#get pbic
pbic1 = presented.ph[ , c(1, 2)]
pbic2 = presented.ph[ , c(1, 4)]

pbic1$encoding = rep("global")
pbic2$encoding = rep("read")

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Username,
        dv = score,
        between = encoding,
        type = 3,
        detailed = T)

##Critical lure items
model2 = ezANOVA(CL,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #higher false recognition of lures for global JOLs

#get MSE here
model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

##post-hocs
#set up the data
cl.ph = cast(CL, Username ~ encoding, mean)

#global vs item
temp = t.test(cl.ph$global, cl.ph$item, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#item vs read
temp = t.test(cl.ph$read, cl.ph$item, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-SIG

#get pbic
pbic1 = cl.ph[ , c(1, 3)]
pbic2 = cl.ph[ , c(1, 4)]

pbic1$encoding = rep("item")
pbic2$encoding = rep("read")

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Username,
        dv = score,
        between = encoding,
        type = 3,
        detailed = T)

#global vs read
temp = t.test(cl.ph$read, cl.ph$global, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG

#get d's
mean(cl.ph$global, na.rm = T); mean(cl.ph$read, na.rm = T)
sd(cl.ph$global, na.rm = T); sd(cl.ph$read, na.rm = T)

#control items
ezANOVA(control,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #no differences (as expected)
