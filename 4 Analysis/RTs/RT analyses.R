####read in the data####
dat1 = read.csv("ex1a encoding final.csv")
dat2 = read.csv("ex1b encoding final.csv")
dat3 = read.csv("ex2 encoding final.csv")

##collapse across list types
library(reshape)

dat1.long = melt(dat1, measure.vars = c("Related", "Unrelated"))
dat2.long = melt(dat2, measure.vars = c("Related", "Unrelated"))
dat3.long = melt(dat3, measure.vars = "V1")

combined = rbind(dat1.long, dat2.long, dat3.long)

colnames(combined)[4:5] = c("Direction", "RT")

##run the anova
library(ez)
options(scipen = 999)

combined = na.omit(combined)

model = ezANOVA(combined,
                dv = RT,
                between = .(Encoding, experiment),
                wid = Username,
                type = 3,
                detailed = T)
model

tapply(combined$RT, combined$Encoding, mean)

##okay, break it down
item = subset(combined, combined$Encoding == "item")
global = subset(combined, combined$Encoding == "global")
read = subset(combined, combined$Encoding == "read")

t.test(read$RT, global$RT, paired = F, var.equal = T)
t.test(item$RT, global$RT, paired = F, var.equal = T)
t.test(item$RT, read$RT, paired = F, var.equal = T)
