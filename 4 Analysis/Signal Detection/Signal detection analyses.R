####Read in the data####
ex1 = read.csv("Sig Data/ex1_sig.csv")
ex2_p = read.csv("Sig Data/ex2_sig_pres.csv")
ex2_f = read.csv("Sig Data/ex2_sig_false.csv")

##turn off scientific notation
options(scipen = 999)

#load libraries
library(ez)

####Ex 2####
###Make Encoding group Subsets
iJOL = subset(ex1,
              ex1$e == "Item_JOL")
gJOL = subset(ex1,
              ex1$e == "Global_JOL")
Read = subset(ex1,
              ex1$e == "Read")

###get means
mean(iJOL$dprime); mean(gJOL$dprime); mean(Read$dprime) #2.76 vs. 1.98 vs. 1.91
mean(iJOL$c.1); mean(gJOL$c.1); mean(Read$c.1) #.03 vs. .22 vs. .16  

#get sds
sd(iJOL$dprime); sd(gJOL$dprime); sd(Read$dprime) 
sd(iJOL$c.1); sd(gJOL$c.1); sd(Read$c.1) 

##d'
#ANOVA
ezANOVA(ex1,
        wid = i,
        between = e,
        dv = dprime,
        type = 3,
        detailed = T) #sig

#post hoc
temp = t.test(iJOL$dprime, gJOL$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(iJOL$dprime, Read$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(gJOL$dprime, Read$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#Only Item JOLs improve discriminability

##C
ezANOVA(ex1,
        wid = i,
        between = e,
        dv = c.1,
        type = 3,
        detailed = T) #marginal

temp = t.test(iJOL$c.1, gJOL$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic  #sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(iJOL$c.1, Read$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(gJOL$c.1, Read$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#####Ex 2 -- Presented####
###Make Encoding group Subsets
iJOL2 = subset(ex2_p,
               ex2_p$e == "item")
gJOL2 = subset(ex2_p,
               ex2_p$e == "global")
Read2 = subset(ex2_p,
               ex2_p$e == "read")

###get means
mean(iJOL2$dprime); mean(gJOL2$dprime); mean(Read2$dprime) #2.05 vs 1.71 vs 1.64
mean(iJOL2$c.1); mean(gJOL2$c.1); mean(Read2$c.1) #-0.10 vs 0.11 vs .23

#get sds
sd(iJOL2$dprime); sd(gJOL2$dprime); sd(Read2$dprime) 
sd(iJOL2$c.1); sd(gJOL2$c.1); sd(Read2$c.1) 

###d'
##ANOVA
ezANOVA(ex2_p,
        wid = i,
        between = e,
        dv = dprime,
        type = 3,
        detailed = T) #marginal

#post hocs
temp = t.test(iJOL2$dprime, gJOL2$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #marginal
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(iJOL2$dprime, Read2$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #barely sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(gJOL2$dprime, Read2$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##C
#ANOVA
ezANOVA(ex2_p,
        wid = i,
        between = e,
        dv = c.1,
        type = 3,
        detailed = T) #Sig

#Post-hocs
temp = t.test(iJOL2$c.1, gJOL2$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

temp = t.test(iJOL2$c.1, Read2$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

temp = t.test(gJOL2$c.1, Read2$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

####Ex 2 -- False####
##Make Encoding group Subsets
iJOL3 = subset(ex2_f,
               ex2_f$e == "item")
gJOL3 = subset(ex2_f,
               ex2_f$e == "global")
Read3 = subset(ex2_f,
               ex2_f$e == "read")

###get means
mean(iJOL3$dprime); mean(gJOL3$dprime); mean(Read3$dprime) #.1.04 vs. 1.35 vs. 1.03
mean(iJOL3$c.1); mean(gJOL3$c.1); mean(Read3$c.1) #.18 vs -0.04 vs 0.16

#get sds
sd(iJOL3$dprime); sd(gJOL3$dprime); sd(Read3$dprime) 
sd(iJOL3$c.1); sd(gJOL3$c.1); sd(Read3$c.1) 

###d'
##ANOVA
ezANOVA(ex2_p,
        wid = i,
        between = e,
        dv = dprime,
        type = 3,
        detailed = T) #marginal

#post-hocs
temp = t.test(iJOL3$dprime, gJOL3$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #marginal
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(iJOL3$dprime, Read3$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #barely sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(gJOL3$dprime, Read3$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##C
#ANOVA
ezANOVA(ex2_p,
        wid = i,
        between = e,
        dv = c.1,
        type = 3,
        detailed = T) #Sig

#Post-hocs
temp = t.test(iJOL3$c.1, gJOL3$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

temp = t.test(iJOL3$c.1, Read3$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

temp = t.test(gJOL3$c.1, Read3$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C