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

#lower case keys
KEYA$Related = tolower(KEYA$Related)
KEYA$Unrelated = tolower(KEYA$Unrelated)

KEYB$Related = tolower(KEYB$Related)
KEYB$Unrelated = tolower(KEYB$Unrelated)

####Finish setting up free recall data for scoring
##Start w/ read group
read_dat = subset(read,
                  read$Procedure.Trial.Type == "FreeRecall")

#fix missing coding
read_dat$Procedure.Procedure.Notes[read_dat$Procedure.Procedure.Notes == ""] = "Unrelated"

#fix puncuation
read_dat$Response.Response = gsub(",", "", read_dat$Response.Response)

##make responses lowercase
read_dat$Response.Response = tolower(read_dat$Response.Response)

##fix weirdness
read_dat$Response.Response[read_dat$Response.Response == "mayorstudentballetpants jeepdollarguitar"] = "mayor student ballet pants jeep dollar guitar"

#get free-recall into long format
read.long = arrange_data(read_dat, "Response.Response", sep = " ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle"))

##okay, relatedness codings for C & D get inverted. Time to fix
#keep
read_long1 = subset(read.long, read.long$Condition.Description == "Read A" | read.long$Condition.Description == "Read B")

#change
read_long2 = subset(read.long, read.long$Condition.Description == "Read C" | read.long$Condition.Description == "Read D")

read_long2$Procedure.Procedure.Notes[read_long2$Procedure.Procedure.Notes == "Related"] = "Unrelated1"
read_long2$Procedure.Procedure.Notes[read_long2$Procedure.Procedure.Notes == "Unrelated"] = "Related"
read_long2$Procedure.Procedure.Notes[read_long2$Procedure.Procedure.Notes == "Unrelated1"] = "Unrelated"

#put back together
read.long = rbind(read_long1, read_long2)

###Okay, now score each list separately
read.A = subset(read.long,
                  read.long$Condition.Description == "Read A")
read.B = subset(read.long,
                read.long$Condition.Description == "Read B")
read.C = subset(read.long,
                read.long$Condition.Description == "Read C")
read.D = subset(read.long,
                read.long$Condition.Description == "Read D")

##Now subset each by test
#Start w/ read A
read.A1 = subset(read.A,
                 read.A$Procedure.Procedure.Notes == "Related")
read.A2 = subset(read.A,
                 read.A$Procedure.Procedure.Notes == "Unrelated")

#Now read B
read.B1 = subset(read.B,
                 read.B$Procedure.Procedure.Notes == "Related")
read.B2 = subset(read.B,
                 read.B$Procedure.Procedure.Notes == "Unrelated")

#Now read C
read.C1 = subset(read.C,
                 read.C$Procedure.Procedure.Notes == "Related")
read.C2 = subset(read.C,
                 read.C$Procedure.Procedure.Notes == "Unrelated")

#Now read D
read.D1 = subset(read.D,
                 read.D$Procedure.Procedure.Notes == "Related")
read.D2 = subset(read.D,
                 read.D$Procedure.Procedure.Notes == "Unrelated")

##Start w/A
#Start scoring
ScoredA1 = prop_correct_free(read.A1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredA2 = prop_correct_free(read.A2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredA = rbind(ScoredA1$DF_Participant, ScoredA2$DF_Participant)

#Now B
ScoredB1 = prop_correct_free(read.B1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredB2 = prop_correct_free(read.B2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredB = rbind(ScoredB1$DF_Participant, ScoredB2$DF_Participant)

#And C
ScoredC1 = prop_correct_free(read.C1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredC2 = prop_correct_free(read.C2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredC = rbind(ScoredC1$DF_Participant, ScoredC2$DF_Participant)

#And D
ScoredD1 = prop_correct_free(read.D1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredD2 = prop_correct_free(read.D2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredD = rbind(ScoredD1$DF_Participant, ScoredD2$DF_Participant)

####Combine all read
Read_Scored = rbind(scoredA, scoredB, scoredC,scoredD)

####Now item group####
item_dat = subset(item,
                  item$Procedure.Trial.Type == "FreeRecall")

#fix missing coding
item_dat$Procedure.Procedure.Notes[item_dat$Procedure.Procedure.Notes == ""] = "Unrelated"

#fix puncuation
item_dat$Response.Response = gsub(",", "", item_dat$Response.Response)

##make responses lowercase
item_dat$Response.Response = tolower(item_dat$Response.Response)

#fix weirdness
item_dat$Response.Response[item_dat$Response.Response == "canyonoceanvalley.rock rivermountain glacier "] = "canyon ocean valley rock river mountain glacier"
item_dat$Response.Response[item_dat$Response.Response == "rockmountainvolcano hillocean "] = "rock mountain volcano hill ocean"
item_dat$Response.Response[item_dat$Response.Response == "milkrifle student guitar oxygen jeep "] = "milk rifle student guitar oxygen jeep"

#get free-recall into long format
item.long = arrange_data(item_dat, "Response.Response", sep = " ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle"))

##okay, relatedness codings for C & D get inverted. Time to fix
#keep
item_long1 = subset(item.long, item.long$Condition.Description == "Item A" | item.long$Condition.Description == "Item B")

#change
item_long2 = subset(item.long, item.long$Condition.Description == "Item C" | item.long$Condition.Description == "Item D")

item_long2$Procedure.Procedure.Notes[item_long2$Procedure.Procedure.Notes == "Related"] = "Unrelated1"
item_long2$Procedure.Procedure.Notes[item_long2$Procedure.Procedure.Notes == "Unrelated"] = "Related"
item_long2$Procedure.Procedure.Notes[item_long2$Procedure.Procedure.Notes == "Unrelated1"] = "Unrelated"

#put back together
item.long = rbind(item_long1, item_long2)

###Okay, now score each list separately
item.A = subset(item.long,
                item.long$Condition.Description == "Item A")
item.B = subset(item.long,
                item.long$Condition.Description == "Item B")
item.C = subset(item.long,
                item.long$Condition.Description == "Item C")
item.D = subset(item.long,
                item.long$Condition.Description == "Item D")

##Now subset each by test
#Start w/ item A
item.A1 = subset(item.A,
                 item.A$Procedure.Procedure.Notes == "Related")
item.A2 = subset(item.A,
                 item.A$Procedure.Procedure.Notes == "Unrelated")

#Now item B
item.B1 = subset(item.B,
                 item.B$Procedure.Procedure.Notes == "Related")
item.B2 = subset(item.B,
                 item.B$Procedure.Procedure.Notes == "Unrelated")

#Now item C
item.C1 = subset(item.C,
                 item.C$Procedure.Procedure.Notes == "Related")
item.C2 = subset(item.C,
                 item.C$Procedure.Procedure.Notes == "Unrelated")

#Now item D
item.D1 = subset(item.D,
                 item.D$Procedure.Procedure.Notes == "Related")
item.D2 = subset(item.D,
                 item.D$Procedure.Procedure.Notes == "Unrelated")

##Start w/A
#Start scoring
ScoredA1 = prop_correct_free(item.A1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredA2 = prop_correct_free(item.A2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredA = rbind(ScoredA1$DF_Participant, ScoredA2$DF_Participant)

#Now B
ScoredB1 = prop_correct_free(item.B1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredB2 = prop_correct_free(item.B2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredB = rbind(ScoredB1$DF_Participant, ScoredB2$DF_Participant)

#And C
ScoredC1 = prop_correct_free(item.C1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredC2 = prop_correct_free(item.C2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredC = rbind(ScoredC1$DF_Participant, ScoredC2$DF_Participant)

#And D
ScoredD1 = prop_correct_free(item.D1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredD2 = prop_correct_free(item.D2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredD = rbind(ScoredD1$DF_Participant, ScoredD2$DF_Participant)

####Combine all ITEM
item_Scored = rbind(scoredA, scoredB, scoredC,scoredD)

####And finally global####
glob_dat = subset(glob,
                  glob$Procedure.Trial.Type == "FreeRecall")

#fix missing coding
glob_dat$Procedure.Procedure.Notes[glob_dat$Procedure.Procedure.Notes == ""] = "Unrelated"

#fix puncuation
glob_dat$Response.Response = gsub(",", "", glob_dat$Response.Response)

##make responses lowercase
glob_dat$Response.Response = tolower(glob_dat$Response.Response)

#fix weirdness
glob_dat$Response.Response[glob_dat$Response.Response == "kayakyellowmintbrickredblaze.road"] = "kayak yellow mint brick red blaze road"
glob_dat$Response.Response[glob_dat$Response.Response == "plumstrwaberrypeachkingdinechefliceelasticbungee"] = "plum strawberry peach king dine chef lice elastic bungee"
glob_dat$Response.Response[glob_dat$Response.Response == "drillmothernailbornfatherkillerthrillermaidsingle"] = "drill mother nail born father killer maid single"
glob_dat$Response.Response[glob_dat$Response.Response == "baileyshipwatericeicebergbraveringerlovegum"] = "bailey ship water ice iceberg brave ringer love gum"

#get free-recall into long format
glob.long = arrange_data(glob_dat, "Response.Response", sep = " ", id = "Username", repeated = c("Condition.Description", "Procedure.Procedure.Notes", "Procedure.Shuffle"))

##okay, relatedness codings didn't get inverted this time
#keep
#glob_long1 = subset(glob.long, glob.long$Condition.Description == "Global A" | glob.long$Condition.Description == "Global B")

#change
#glob_long2 = subset(glob.long, glob.long$Condition.Description == "Global C" | glob.long$Condition.Description == "Global D")

#glob_long2$Procedure.Procedure.Notes[glob_long2$Procedure.Procedure.Notes == "Related"] = "Unrelated1"
#glob_long2$Procedure.Procedure.Notes[glob_long2$Procedure.Procedure.Notes == "Unrelated"] = "Related"
#glob_long2$Procedure.Procedure.Notes[glob_long2$Procedure.Procedure.Notes == "Unrelated1"] = "Unrelated"

#put back together
#glob.long = rbind(glob_long1, glob_long2)

###Okay, now score each list separately
glob.A = subset(glob.long,
                glob.long$Condition.Description == "Global A")
glob.B = subset(glob.long,
                glob.long$Condition.Description == "Global B")
glob.C = subset(glob.long,
                glob.long$Condition.Description == "Global C")
glob.D = subset(glob.long,
                glob.long$Condition.Description == "Global D")

##Now subset each by test
#Start w/ glob A
glob.A1 = subset(glob.A,
                 glob.A$Procedure.Procedure.Notes == "Related")
glob.A2 = subset(glob.A,
                 glob.A$Procedure.Procedure.Notes == "Unrelated")

#Now glob B
glob.B1 = subset(glob.B,
                 glob.B$Procedure.Procedure.Notes == "Related")
glob.B2 = subset(glob.B,
                 glob.B$Procedure.Procedure.Notes == "Unrelated")

#Now glob C
glob.C1 = subset(glob.C,
                 glob.C$Procedure.Procedure.Notes == "Related")
glob.C2 = subset(glob.C,
                 glob.C$Procedure.Procedure.Notes == "Unrelated")

#Now glob D
glob.D1 = subset(glob.D,
                 glob.D$Procedure.Procedure.Notes == "Related")
glob.D2 = subset(glob.D,
                 glob.D$Procedure.Procedure.Notes == "Unrelated")

##Start w/A
#Start scoring
ScoredA1 = prop_correct_free(glob.A1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredA2 = prop_correct_free(glob.A2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredA = rbind(ScoredA1$DF_Participant, ScoredA2$DF_Participant)

#Now B
ScoredB1 = prop_correct_free(glob.B1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredB2 = prop_correct_free(glob.B2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredB = rbind(ScoredB1$DF_Participant, ScoredB2$DF_Participant)

##And C
ScoredC1 = prop_correct_free(glob.C1, responses = "response", id = "Sub.ID", key = KEYA$Related, cutoff = 1)
ScoredC2 = prop_correct_free(glob.C2, responses = "response", id = "Sub.ID", key = KEYA$Unrelated, cutoff = 1)

#put it back together
scoredC = rbind(ScoredC1$DF_Participant, ScoredC2$DF_Participant)

#And D
ScoredD1 = prop_correct_free(glob.D1, responses = "response", id = "Sub.ID", key = KEYB$Related, cutoff = 1)
ScoredD2 = prop_correct_free(glob.D2, responses = "response", id = "Sub.ID", key = KEYB$Unrelated, cutoff = 1)

#put it back together
scoredD = rbind(ScoredD1$DF_Participant, ScoredD2$DF_Participant)

####Combine all ITEM
glob_Scored = rbind(scoredA, scoredB, scoredC,scoredD)

#####Now combine everything####
glob_Scored$encoding = rep("global")
Read_Scored$encoding = rep("read")
item_Scored$encoding = rep("item")

Ex1A_scored = rbind(glob_Scored, Read_Scored, item_Scored)

##drop low effort participants
#global
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "5c4e5f2ae5f00f0001542748")
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "63fbd3e8b4865c6e1fb04614")
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "62fbe4c86d484357b6adbc36")

#item
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "652d5dcfbf51f8f449531f8d")
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "63f7dc10de20707c3dff50b4")
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "5d8a29c082fec30001d9c24a")

#read
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "650215217993641ac7ab92a8")
###Remove cheaters
#read
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "599a9252bbe848000179676e")
Ex1A_scored = subset(Ex1A_scored,
                     Ex1A_scored$Sub.ID != "5788c16f275be600013590a8")


##Write to file
#write.csv(Ex1A_scored, file = "ex1a_fixed.csv", row.names = F)
