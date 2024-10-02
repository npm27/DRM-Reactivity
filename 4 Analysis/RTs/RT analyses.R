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

####Get individual experiments####
##subset
dat1.long = subset(combined, combined$experiment == "1A")
dat2.long = subset(combined, combined$experiment == "1B")
dat3.long = subset(combined, combined$experiment == "2")

##means
tapply(dat1.long$RT, list(dat1.long$Encoding, dat1.long$Direction), mean)
tapply(dat2.long$RT, list(dat2.long$Encoding, dat2.long$Direction), mean)
tapply(dat3.long$RT, list(dat3.long$Encoding, dat3.long$Direction), mean)

##get CIs
#Ex 1A
d1.I = cast(subset(dat1.long,
            dat1.long$Encoding == "item"),
            Username ~ Direction, mean, value = "RT")

(apply(d1.I, 2, sd) / sqrt(nrow(d1.I))) * 1.96

d1.G = cast(subset(dat1.long,
                   dat1.long$Encoding == "global"),
            Username ~ Direction, mean, value = "RT")

(apply(d1.G, 2, sd) / sqrt(nrow(d1.G))) * 1.96

d1.R = cast(subset(dat1.long,
                   dat1.long$Encoding == "read"),
            Username ~ Direction, mean, value = "RT")

(apply(d1.R, 2, sd) / sqrt(nrow(d1.R))) * 1.96

#Ex 1B
d2.I = cast(subset(dat2.long,
                   dat2.long$Encoding == "item"),
            Username ~ Direction, mean, value = "RT")

(apply(d2.I, 2, sd) / sqrt(nrow(d2.I))) * 1.96

d2.G = cast(subset(dat2.long,
                   dat2.long$Encoding == "global"),
            Username ~ Direction, mean, value = "RT")

(apply(d2.G, 2, sd, na.rm = T) / sqrt(nrow(d2.G))) * 1.96

d2.R = cast(subset(dat2.long,
                   dat2.long$Encoding == "read"),
            Username ~ Direction, mean, value = "RT")

(apply(d2.R, 2, sd, na.rm = T) / sqrt(nrow(d2.R))) * 1.96

#Ex 2
d3.I = cast(subset(dat3.long,
                   dat3.long$Encoding == "item"),
            Username ~ Direction, mean, value = "RT")

(sd(d3.I$V1) / sqrt(nrow(d3.I))) * 1.96

d3.G = cast(subset(dat3.long,
                   dat3.long$Encoding == "global"),
            Username ~ Direction, mean, value = "RT")

(sd(d3.G$V1) / sqrt(nrow(d3.I))) * 1.96

d3.R = cast(subset(dat3.long,
                   dat3.long$Encoding == "read"),
            Username ~ Direction, mean, value = "RT")

(sd(d3.R$V1) / sqrt(nrow(d3.I))) * 1.96
