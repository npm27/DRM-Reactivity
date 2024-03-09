####Set up####
##load libraries
library(lrd)
library(dplyr)
library(reshape)

##Load data
item = read.csv("Data/Item.csv")
glob = read.csv("Data/Global.csv")
read = read.csv("Data/Read.csv")

#load keys
KEYA = read.csv("Keys/KeysA.csv")
KEYB = read.csv("Keys/KeysB.csv")

KEYA.DRM = KEYA[ c(1:10), c(1, 3)]
KEYB.DRM = KEYB[ c(1:10), c(1, 3)]

####Finish setting up free recall data for scoring
####Start w/ read group####
read_dat = subset(read,
                  read$Procedure.Trial.Type == "FreeRecall")

#fix punctuation
read_dat$Response.Response = gsub(",", ", ", read_dat$Response.Response)
read_dat$Response.Response = gsub(",  ", ", ", read_dat$Response.Response)

##make responses lowercase
read_dat$Response.Response = tolower(read_dat$Response.Response)

#get free-recall into long format
read.long = arrange_data(read_dat, "Response.Response", sep = ", ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle", "Condition.Notes"))

###Okay, now score each list separately
read.A = subset(read.long,
                read.long$Condition.Notes == "A")
read.B = subset(read.long,
                read.long$Condition.Notes == "B")

###Start scoring
##Start w/A
ScoredA = prop_correct_free(read.A, responses = "response", id = "Sub.ID", key = KEYA$Key1, cutoff = 1)
ScoredA.DRM = prop_correct_free(read.A, responses = "response", id = "Sub.ID", key = KEYA.DRM$DRM, cutoff = 1)

#now B
ScoredB = prop_correct_free(read.B, responses = "response", id = "Sub.ID", key = KEYB$Key1, cutoff = 1)
ScoredB.DRM = prop_correct_free(read.B, responses = "response", id = "Sub.ID", key = KEYB.DRM$DRM, cutoff = 1)

##put it together
Read_Scored = rbind(ScoredA$DF_Participant, ScoredB$DF_Participant)
Read_Scored.DRM = rbind(ScoredA.DRM$DF_Participant, ScoredB.DRM$DF_Participant)

####Now the item group####
item_dat = subset(item,
                  item$Procedure.Trial.Type == "FreeRecall")

#fix punctuation
item_dat$Response.Response = gsub(",", ", ", item_dat$Response.Response)
item_dat$Response.Response = gsub(",  ", ", ", item_dat$Response.Response)
item_dat$Response.Response = gsub(". ", ", ", item_dat$Response.Response)
item_dat$Response.Response = gsub(", ", " ", item_dat$Response.Response)

##make responses lowercase
item_dat$Response.Response = tolower(item_dat$Response.Response)

#get free-recall into long format
item.long = arrange_data(item_dat, "Response.Response", sep = " ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle", "Condition.Notes"))

###Okay, now score each list separately
item.A = subset(item.long,
                item.long$Condition.Notes == "A")
item.B = subset(item.long,
                item.long$Condition.Notes == "B")

###Start scoring
##Start w/A
ScoredA = prop_correct_free(item.A, responses = "response", id = "Sub.ID", key = KEYA$Key1, cutoff = 1)
ScoredA.DRM = prop_correct_free(item.A, responses = "response", id = "Sub.ID", key = KEYA.DRM$DRM, cutoff = 1)

#now B
ScoredB = prop_correct_free(item.B, responses = "response", id = "Sub.ID", key = KEYB$Key1, cutoff = 1)
ScoredB.DRM = prop_correct_free(item.B, responses = "response", id = "Sub.ID", key = KEYB.DRM$DRM, cutoff = 1)

##put it together
Item_Scored = rbind(ScoredA$DF_Participant, ScoredB$DF_Participant)
Item_Scored.DRM = rbind(ScoredA.DRM$DF_Participant, ScoredB.DRM$DF_Participant)

####Now do the global group####
glob_dat = subset(glob,
                  glob$Procedure.Trial.Type == "FreeRecall")

#fix punctuation
glob_dat$Response.Response = gsub(",", ", ", glob_dat$Response.Response)
glob_dat$Response.Response = gsub(",  ", ", ", glob_dat$Response.Response)
glob_dat$Response.Response = gsub(". ", ", ", glob_dat$Response.Response)
glob_dat$Response.Response = gsub(", ", " ", glob_dat$Response.Response)

##make responses lowercase
glob_dat$Response.Response = tolower(glob_dat$Response.Response)

#get free-recall into long format
glob.long = arrange_data(glob_dat, "Response.Response", sep = " ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle", "Condition.Notes"))

###Okay, now score each list separately
glob.A = subset(glob.long,
                glob.long$Condition.Notes == "A")
glob.B = subset(glob.long,
                glob.long$Condition.Notes == "B")

###Start scoring
##Start w/A
ScoredA = prop_correct_free(glob.A, responses = "response", id = "Sub.ID", key = KEYA$Key1, cutoff = 1)
ScoredA.DRM = prop_correct_free(glob.A, responses = "response", id = "Sub.ID", key = KEYA.DRM$DRM, cutoff = 1)

#now B
ScoredB = prop_correct_free(glob.B, responses = "response", id = "Sub.ID", key = KEYB$Key1, cutoff = 1)
ScoredB.DRM = prop_correct_free(glob.B, responses = "response", id = "Sub.ID", key = KEYB.DRM$DRM, cutoff = 1)

##put it together
glob_Scored = rbind(ScoredA$DF_Participant, ScoredB$DF_Participant)
glob_Scored.DRM = rbind(ScoredA.DRM$DF_Participant, ScoredB.DRM$DF_Participant)

####Add column codings and combine####
##Column codings
glob_Scored$task = rep("global")
glob_Scored.DRM$task = rep("global")

Item_Scored$task = rep("item")
Item_Scored.DRM$task = rep("item")

Read_Scored$task = rep("read")
Read_Scored.DRM$task = rep("read")

glob_Scored$mem_type = rep("true")
glob_Scored.DRM$mem_type = rep("DRM")

Item_Scored$mem_type = rep("true")
Item_Scored.DRM$mem_type = rep("DRM")

Read_Scored$mem_type = rep("true")
Read_Scored.DRM$mem_type = rep("DRM")

##Combine
final = rbind(glob_Scored, glob_Scored.DRM,
              Item_Scored, Item_Scored.DRM,
              Read_Scored, Read_Scored.DRM)

##write to .csv
#write.csv(final, file = "ex2a.csv", row.names = F)
