####Set up####
##read in data
dat = read.csv("Data/ex1a.csv")

##load libraries
library(reshape)
library(ez)

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

#remove participant who didn't complete experiment
dat = subset(dat,
             dat$ID != "6563003ce4f5a268a959df0")

item = subset(dat,
              dat$encoding == "item")
table(item$ID)

#remove participant who didn't complete experiment
dat = subset(dat,
             dat$ID != "653b9ed2c4b3466a16bc6e05")

length(unique(item$ID)) #38 - 1

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

ezANOVA(dat,
        wid = ID,
        within = List_Type,
        between = encoding,
        dv = scored,
        type = 3,
        detailed = T)

##marginal main effect encoding
##signicant effect list type
##significant interaction
