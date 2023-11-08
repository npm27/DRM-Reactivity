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

####Build JOL data####
