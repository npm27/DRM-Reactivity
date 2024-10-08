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
read.wide = cast(read, ID ~ List_Type, mean) #Nope, looks good!

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

####Any interactions w/ version?####
vs = read.csv("1a versions.csv")

dat.vs = merge(dat, vs, by.x = "ID", by.y = "Username")

colnames(dat.vs)[5] = "version"

model.v = ezANOVA(dat.vs,
                 wid = ID,
                 within = List_Type,
                 between = .(encoding, version),
                 dv = scored,
                 type = 3,
                 detailed = T)

model.v #no interactions or main effects of sequence

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
#global vs read
temp = t.test(global.wide$Related, item.wide$Related, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

sd(global.wide$Related); sd(item.wide$Related)

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

sd(item.wide$Unrelated); sd(read.wide$Unrelated)

##get 95% CI values for Table A2
(apply(item.wide, 2, sd) / sqrt(nrow(item.wide))) * 1.96
(apply(global.wide, 2, sd) / sqrt(nrow(global.wide))) * 1.96
(apply(read.wide, 2, sd) / sqrt(nrow(read.wide))) * 1.96

####Get JOLs for Table A4
JOLs = read.csv("ex1a JOLs.csv")

##drop JOL outliers
JOLs = subset(JOLs,
              JOLs$Username != "653b9ed2c4b3466a16bc6e05")

#these were dropped from the recall data during the scoring phase
JOLs = subset(JOLs,
                  JOLs$Username != "5c4e5f2ae5f00f0001542748")
JOLs = subset(JOLs,
                  JOLs$Username != "63fbd3e8b4865c6e1fb04614")
JOLs = subset(JOLs,
                  JOLs$Username != "62fbe4c86d484357b6adbc36")
JOLs = subset(JOLs,
                  JOLs$Username != "652d5dcfbf51f8f449531f8d")
JOLs = subset(JOLs,
                  JOLs$Username != "63f7dc10de20707c3dff50b4")
JOLs = subset(JOLs,
                  JOLs$Username != "5d8a29c082fec30001d9c24a")
JOLs = subset(JOLs,
           JOLs$Username != "650215217993641ac7ab92a8")
JOLs = subset(JOLs,
           JOLs$Username != "599a9252bbe848000179676e")
JOLs = subset(JOLs,
           JOLs$Username != "5788c16f275be600013590a8")
JOLs = subset(JOLs,
           JOLs$Username != "561487ad7ffc8a0012812266" & JOLs$Username != "5fc653586deb34268015308a")
JOLs = subset(JOLs,
           JOLs$Username != "65730b470b3ff6af4dfe899d" & JOLs$Username != "650f1cffdfb4dd61f51abefd" &
             JOLs$Username != "6563003ce4f5a268a959df06")
JOLs = subset(JOLs,
           JOLs$Username != "5bde0463520c030001f4959f" & JOLs$Username != "657c791622f9617fcc0de8fa")

##Now get means and CIs s for table A4
tapply(JOLs$Related, JOLs$encoding, mean, na.rm = T)
tapply(JOLs$Unrelated, JOLs$encoding, mean, na.rm = T)

JOLs.I = subset(JOLs, JOLs$encoding == "item")
JOLs.G = subset(JOLs, JOLs$encoding == "global")

(apply(JOLs.I[ , c(2:3)], 2, sd, na.rm = T) / sqrt(nrow(JOLs.I))) * 1.96
(apply(JOLs.G[ , c(2:3)], 2, sd, na.rm = T) / sqrt(nrow(JOLs.G))) * 1.96

t.test(JOLs.I$Related, JOLs.I$Unrelated, paired = T, var.equal = T)
t.test(JOLs.G$Related, JOLs.G$Unrelated, paired = T, var.equal = T)

####Encoding latencies####
e = read.csv("Ex1a Encoding.csv")

##drop outliers
e = subset(e,
              e$Username != "653b9ed2c4b3466a16bc6e05")

#these were dropped from the recall data during the scoring phase
e = subset(e,
              e$Username != "5c4e5f2ae5f00f0001542748")
e = subset(e,
              e$Username != "63fbd3e8b4865c6e1fb04614")
e = subset(e,
              e$Username != "62fbe4c86d484357b6adbc36")
e = subset(e,
              e$Username != "652d5dcfbf51f8f449531f8d")
e = subset(e,
              e$Username != "63f7dc10de20707c3dff50b4")
e = subset(e,
              e$Username != "5d8a29c082fec30001d9c24a")
e = subset(e,
           e$Username != "650215217993641ac7ab92a8")
e = subset(e,
           e$Username != "599a9252bbe848000179676e")
e = subset(e,
           e$Username != "5788c16f275be600013590a8")
e = subset(e,
           e$Username != "561487ad7ffc8a0012812266" & e$Username != "5fc653586deb34268015308a")
e = subset(e,
           e$Username != "65730b470b3ff6af4dfe899d" & e$Username != "650f1cffdfb4dd61f51abefd" &
           e$Username != "6563003ce4f5a268a959df06")
e = subset(e,
           e$Username != "5bde0463520c030001f4959f" & e$Username != "657c791622f9617fcc0de8fa")

####Run the Anova####
library(ez)
library(reshape)
options(scipen = 999)

e.long = melt(e,
              measure.vars = c("Related", "Unrelated"))
colnames(e.long)[3:4] = c("List_Type", "Score")

e.long = na.omit(e.long)

ezANOVA(e.long,
        wid = Username,
        dv = Score,
        between = Encoding,
        within = List_Type,
        type = 3,
        detailed = T)

tapply(e.long$Score, list(e.long$Encoding, e.long$List_Type), mean)

e$experiment = rep("1A")
#write.csv(e, file = "ex1a encoding final.csv", row.names = F)