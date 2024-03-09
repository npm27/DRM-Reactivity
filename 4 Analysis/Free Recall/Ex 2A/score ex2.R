####Set up####
##read in data
dat = read.csv("Data/ex2a.csv")

##load libraries
library(reshape)
library(ez)

options(scipen = 999)

##fix column names, drop unused columns, and reorder
colnames(dat)[2:3] = c("score", "group")

dat = dat[ , -c(4:6)]

dat = dat[ , c(1, 3, 5, 2)]

dat$score[dat$score > 1] = 1

##how many participants?
table(dat$group) / 2 #41 global, 42 item, 40 read (might need to get these numbers up a bit)

##data screening
item = subset(dat,
              dat$group == "Item")

##subset out by memory type
correct = subset(dat,
                 dat$mem_type == 'true')
drm = subset(dat,
            dat$mem_type == "DRM")

##get data patterns
tapply(correct$score, correct$group, mean, na.rm = T)
tapply(drm$score, drm$group, mean, na.rm = T)

####ANOVA####
ezANOVA(dat,
        dv = score,
        wid = Sub.ID,
        between = group,
        within = mem_type,
        type = 3,
        detailed = T)
