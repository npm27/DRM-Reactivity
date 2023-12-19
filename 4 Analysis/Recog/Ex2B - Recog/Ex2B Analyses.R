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
ezANOVA(presented,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #higher correct recognition for item JOLs

##Critical lure items
ezANOVA(CL,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #higher false recognition of lures for global JOLs

#control items
ezANOVA(control,
        dv = scored,
        between = encoding,
        wid = Username,
        type = 3,
        detailed = T) #no differences (as expected)

####Run the post-hocs####
##Presented items

##CL
tapply(presented$scored, presented$encoding, mean)
tapply(CL$scored, CL$encoding, mean)
tapply(control$scored, control$encoding, mean)
