####This script will be used to read in everything and set EX 2data up for processing####
##Start by gathering all of the data
#Item
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1B/Item")

files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#List
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1B/List")

files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Read
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1B/Read")

files3 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat3 = do.call(rbind, lapply(files3, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat3$Username))

#Now move back to the original folder
#This is where I'll store the combined final output for scoring
setwd('..')

##get versions and usernames
#v1 = dat[ , c(1, 9)]
#v2 = dat2[ , c(1, 9)]
#v3 = dat3[ , c(1, 9)]

#vs = rbind(v1, v2, v3)

#vs$dupe = duplicated(vs$Username)
#vs2 = subset(vs,
             #vs$dupe == FALSE)

#write.csv(vs2[ , -3], file = "ex1b version.csv", row.names = F)

#get mean JOLs
#library(reshape)

#dat$Response.JOL = as.numeric(dat$Response.JOL)
#dat2$Response.JOL = as.numeric(dat2$Response.JOL)

#dat$Response.JOL[dat$Response.JOL > 100] = NA
#dat2$Response.JOL[dat2$Response.JOL > 100] = NA

#dat1.JOL = cast(dat, Username ~ Stimuli.Stimuli.Notes, value = "Response.JOL", mean, na.rm = T)
#dat2.JOL = cast(dat2, Username ~ Procedure.Procedure.Notes, value = "Response.JOL", mean, na.rm = T)

#dat1.JOL = dat1.JOL[ , -c(2:4)]
#dat2.JOL = dat2.JOL[ , -c(2:9)]

#dat1.JOL$encoding = rep("item")
#dat2.JOL$encoding = rep("global")

#JOLs = rbind(dat1.JOL, dat2.JOL)

#write.csv(JOLs, file = "ex1b JOLs.csv", row.names = F)

####Get Encoding Latencies####
dat.E = subset(dat,
               dat$Procedure.Procedure.Notes == "Related" | dat$Procedure.Procedure.Notes == "Unrelated")
dat2.E = subset(dat2,
                dat2$Procedure.Procedure.Notes == "Related" | dat2$Procedure.Procedure.Notes == "Unrelated")
dat3.E = subset(dat3,
                dat3$Procedure.Procedure.Notes == "Related" | dat3$Procedure.Procedure.Notes == "Unrelated")

table(dat.E$Procedure.Shuffle)

dat.E = subset(dat.E,
               dat.E$Procedure.Shuffle == "R1" | dat.E$Procedure.Shuffle == "R2" |
                 dat.E$Procedure.Shuffle == "U1" | dat.E$Procedure.Shuffle == "U2")

dat2.E = subset(dat2.E,
                dat2.E$Procedure.Shuffle == "R1" | dat2.E$Procedure.Shuffle == "R2" |
                  dat2.E$Procedure.Shuffle == "U1" | dat2.E$Procedure.Shuffle == "U2")

dat3.E = subset(dat3.E,
                dat3.E$Procedure.Shuffle == "R1" | dat3.E$Procedure.Shuffle == "R2" |
                  dat3.E$Procedure.Shuffle == "U1" | dat3.E$Procedure.Shuffle == "U2")

##omit extreme scores
dat.E$Response.RT[dat.E$Response.RT > 9999] = NA
dat.E$Response.RT[dat.E$Response.RT < 1000] = NA

dat2.E$Response.RT[dat2.E$Response.RT > 9999] = NA
dat2.E$Response.RT[dat2.E$Response.RT < 1000] = NA

dat3.E$Response.RT[dat3.E$Response.RT > 9999] = NA
dat3.E$Response.RT[dat3.E$Response.RT < 1000] = NA

library(reshape)

dat1.Encoding = cast(dat.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)
dat2.Encoding = cast(dat2.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)
dat3.Encoding = cast(dat3.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)

dat1.Encoding$Encoding = rep("item")
dat2.Encoding$Encoding = rep("global")
dat3.Encoding$Encoding = rep("read")

#write.csv(rbind(dat1.Encoding, dat2.Encoding, dat3.Encoding), file = "Ex1b Encoding.csv", row.names = F)

####Clean up the data files####
##Drop unused columns
dat = dat[ , -c(2:7, 9:10, 12, 20:23, 28:33, 35)]
dat2 = dat2[ , -c(2:7, 9:10, 12, 20:23, 28:33, 35)]
dat3 = dat3[ , -c(2:7, 9:10, 12, 20:23, 28:34)]

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "Instruct")

#Now remove filler task
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "FreeRecall")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "FreeRecall")

####Fix related vs unrelated codings####
##Item
dat1A = subset(dat,
               dat$Condition.Description == "Item A" | dat$Condition.Description == "Item C")
dat1B = subset(dat,
               dat$Condition.Description == "Item B" | dat$Condition.Description == "Item D")

#start w/ related
related1A = subset(dat1A,
                   dat1A$Stimuli.Stimuli.Notes == "Related")
related1B = subset(dat1B,
                   dat1B$Stimuli.Stimuli.Notes == "Related")

related1A = unique(related1A$Stimuli.Cue)
related1B = unique(related1B$Stimuli.Cue)

related1A = data.frame(related1A)
related1A$Direction = rep("Related")
colnames(related1A)[1] = "Stimuli.Cue"

related1B = data.frame(related1B)
related1B$Direction = rep("Related")
colnames(related1B)[1] = "Stimuli.Cue"

#now unrelated
unrelated1A = subset(dat1A,
                   dat1A$Stimuli.Stimuli.Notes == "Unrelated")
unrelated1B = subset(dat1B,
                   dat1B$Stimuli.Stimuli.Notes == "Unrelated")

unrelated1A = unique(unrelated1A$Stimuli.Cue)
unrelated1B = unique(unrelated1B$Stimuli.Cue)

unrelated1A = data.frame(unrelated1A)
unrelated1A$Direction = rep("Unrelated")
colnames(unrelated1A)[1] = "Stimuli.Cue"

unrelated1B = data.frame(unrelated1B)
unrelated1B$Direction = rep("Unrelated")
colnames(unrelated1B)[1] = "Stimuli.Cue"

##and control items
controlA = subset(dat1A,
                  dat1A$Stimuli.Stimuli.Notes == "Control")
controlB = subset(dat1B,
                  dat1B$Stimuli.Stimuli.Notes == "Control")

controlA = unique(controlA$Stimuli.Cue)
controlB = unique(controlB$Stimuli.Cue)

controlA = data.frame(controlA)
controlA$Direction = rep("Control")
colnames(controlA)[1] = "Stimuli.Cue"

controlB = data.frame(controlB)
controlB$Direction = rep("Control")
colnames(controlB)[1] = "Stimuli.Cue"

#now tag answer key items
keyA = rbind(related1A, unrelated1A, controlA)
keyb = rbind(related1B, unrelated1B, controlB)

###Now get recog test
recogA = subset(dat1A, dat1A$Procedure.Trial.Type == "Likert")
recogB = subset(dat1B, dat1B$Procedure.Trial.Type == "Likert")

recogA1 = merge(recogA, keyA, by = "Stimuli.Cue")
recogB1 = merge(recogB, keyb, by = "Stimuli.Cue")

recog_item = rbind(recogA1, recogB1)
recog_item$Encoding = rep("Item_JOL")

##Now repeat w/Global
##Item
dat2A = subset(dat2,
               dat2$Condition.Description == "Global A" | dat2$Condition.Description == "Global C")
dat2B = subset(dat2,
               dat2$Condition.Description == "Global B" | dat2$Condition.Description == "Global D")

##should just be able to resuse the same key as above
###Now get recog test
recogA = subset(dat2A, dat2A$Procedure.Trial.Type == "Likert")
recogB = subset(dat2B, dat2B$Procedure.Trial.Type == "Likert")

recogA1 = merge(recogA, keyA, by = "Stimuli.Cue")
recogB1 = merge(recogB, keyb, by = "Stimuli.Cue")

recog_global = rbind(recogA1, recogB1)
recog_global$Encoding = rep("Global_JOL")

##and one more time for the read only group
dat3A = subset(dat3,
               dat3$Condition.Description == "Read A" | dat3$Condition.Description == "Read C")
dat3B = subset(dat3,
               dat3$Condition.Description == "Read B" | dat3$Condition.Description == "Read D")

##should just be able to resuse the same key as above
###Now get recog test
recogA = subset(dat3A, dat3A$Procedure.Trial.Type == "Likert")
recogB = subset(dat3B, dat3B$Procedure.Trial.Type == "Likert")

recogA1 = merge(recogA, keyA, by = "Stimuli.Cue")
recogB1 = merge(recogB, keyb, by = "Stimuli.Cue")

recog_read = rbind(recogA1, recogB1)
recog_read$Encoding = rep("Read")

####Score the data####
##Start w/ item
recog_item$Response.Response[recog_item$Response.Response == "NO"] = "Control"
recog_item$Response.Response[recog_item$Response.Response == "YES"] = "Presented"

recog_item$scored = as.numeric(recog_item$Response.Response == recog_item$Stimuli.Stimuli.Notes)

#now for global
recog_global$Response.Response[recog_global$Response.Response == "NO"] = "Control"
recog_global$Response.Response[recog_global$Response.Response == "YES"] = "Presented"

recog_global$scored = as.numeric(recog_global$Response.Response == recog_global$Stimuli.Stimuli.Notes)

#and the read group
recog_read$Response.Response[recog_read$Response.Response == "NO"] = "Control"
recog_read$Response.Response[recog_read$Response.Response == "YES"] = "Presented"

recog_read$scored = as.numeric(recog_read$Response.Response == recog_read$Stimuli.Stimuli.Notes)

####cleanup and write recog data to file####
##drop extra columns
recog_item = recog_item[ , -c(1, 3:4, 6, 8:13, 14)]
recog_global = recog_global[ , -c(1, 3:4, 6, 8:13, 14)]
recog_read = recog_read[ , -c(1, 3:4, 6, 8:13)]

#write.csv(recog_item, file = "JOL Item.csv", row.names = F)
#write.csv(recog_global, file = "JOL Global.csv", row.names = F)
#write.csv(recog_read, file = "Control Group.csv", row.names = F)
