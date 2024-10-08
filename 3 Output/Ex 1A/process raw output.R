####Set up Free Recall data for scoring####
##Start by gathering all of the data
#Item JOLs
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1A/Item")

files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#Global JOLs
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1A/List")

files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Read
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/DRM-Reactivity/3 Output/Ex 1A/Read")

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
            # vs$dupe == FALSE)

#write.csv(vs2[ , -3], file = "1a versions.csv", row.names = F)

#get mean JOLs
#library(reshape)

#dat$Response.JOL = as.numeric(dat$Response.JOL)
#dat2$Response.JOL = as.numeric(dat2$Response.JOL)

#dat$Response.JOL[dat$Response.JOL > 100] = NA
#dat2$Response.JOL[dat2$Response.JOL > 100] = NA

#dat1.JOL = cast(dat, Username ~ Stimuli.Stimuli.Notes, value = "Response.JOL", mean, na.rm = T)
#dat2.JOL = cast(dat2, Username ~ Procedure.Procedure.Notes, value = "Response.JOL", mean, na.rm = T)

#dat1.JOL = dat1.JOL[ , -2]
#dat2.JOL = dat2.JOL[ , -c(2:3)]

#dat1.JOL$encoding = rep("item")
#dat2.JOL$encoding = rep("global")

#JOLs = rbind(dat1.JOL, dat2.JOL)

#write.csv(JOLs, file = "ex1a JOLs.csv", row.names = F)

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

library(reshape)

##omit extreme scores
dat.E$Response.RT[dat.E$Response.RT > 9999] = NA
dat.E$Response.RT[dat.E$Response.RT < 1000] = NA

dat2.E$Response.RT[dat2.E$Response.RT > 9999] = NA
dat2.E$Response.RT[dat2.E$Response.RT < 1000] = NA

dat3.E$Response.RT[dat3.E$Response.RT > 9999] = NA
dat3.E$Response.RT[dat3.E$Response.RT < 1000] = NA

dat1.Encoding = cast(dat.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)
dat2.Encoding = cast(dat2.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)
dat3.Encoding = cast(dat3.E, Username ~ Stimuli.Stimuli.Notes, value = "Response.RT", mean, na.rm = T)

dat1.Encoding$Encoding = rep("item")
dat2.Encoding$Encoding = rep("global")
dat3.Encoding$Encoding = rep("read")

#write.csv(rbind(dat1.Encoding, dat2.Encoding, dat3.Encoding), file = "Ex1a Encoding.csv", row.names = F)

##get global Judgment RTs
dat.g = subset(dat2, dat2$Procedure.Shuffle == "JOL1" | dat2$Procedure.Shuffle == "JOL2" | 
               dat2$Procedure.Shuffle == "JOL3" | dat2$Procedure.Shuffle == "JOL4")



####Clean up the data files####
##Drop unused columns
dat = dat[ , -c(2:4, 6:7, 9:10, 12, 20:22, 27:32, 34)]
dat2 = dat2[ , -c(2:4, 6:7, 9:10, 12, 20:22, 27:32, 34)]
dat3 = dat3[ , -c(2:4, 6:7, 9:10, 12, 20:22, 27:33)]

#Next remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "Instruct")

##remove participants who didn't complete filler
#item
dat = subset(dat,
              dat$Username != "561487ad7ffc8a0012812266" & dat$Username != "5fc653586deb34268015308a")

#global
dat2 = subset(dat2,
              dat2$Username != "65730b470b3ff6af4dfe899d" & dat2$Username != "650f1cffdfb4dd61f51abefd" &
              dat2$Username != "6563003ce4f5a268a959df06")

#read
dat3 = subset(dat3,
              dat3$Username != "5bde0463520c030001f4959f" & dat3$Username != "657c791622f9617fcc0de8fa")

#Now remove filler task
dat = subset(dat,
             dat$Procedure.Shuffle != "FILLER")
dat2 = subset(dat2,
              dat2$Procedure.Shuffle != "FILLER")
dat3 = subset(dat3,
              dat3$Procedure.Shuffle != "FILLER")

####Set the data up for scoring####
##build the keys
Key1 = subset(dat,
              dat$Stimuli.Shuffle == "R1")
length(unique(Key1$Stimuli.Cue)) #24 (12 items, 1 per counterbalance) #A&C; B&D

Key2 = subset(dat,
              dat$Stimuli.Shuffle == "U1")
Key3 = subset(dat,
              dat$Stimuli.Shuffle == "R2")
Key4 = subset(dat,
              dat$Stimuli.Shuffle == "U2")

#List one Keys
Key1A = subset(Key1,
               Key1$Condition.Description == "Item A" | Key1$Condition.Description == "Item C")
Key1B = subset(Key1,
               Key1$Condition.Description == "Item B" | Key1$Condition.Description == "Item D")

#List two keys
Key2A = subset(Key2,
               Key2$Condition.Description == "Item A" | Key2$Condition.Description == "Item C")
Key2B = subset(Key2,
               Key2$Condition.Description == "Item B" | Key2$Condition.Description == "Item D")

#List three keys
Key3A = subset(Key3,
               Key3$Condition.Description == "Item A" | Key3$Condition.Description == "Item C")
Key3B = subset(Key3,
               Key3$Condition.Description == "Item B" | Key3$Condition.Description == "Item D")

#List four keys
Key4A = subset(Key4,
               Key4$Condition.Description == "Item A" | Key4$Condition.Description == "Item C")
Key4B = subset(Key4,
               Key4$Condition.Description == "Item B" | Key4$Condition.Description == "Item D")

##Now get the unique items for each key and order them
Key1A = Key1A[1:12, ];Key1B = Key1B[1:12, ]
Key2A = Key2A[1:12, ];Key2B = Key2B[1:12, ]
Key3A = Key3A[1:12, ];Key3B = Key3B[1:12, ]
Key4A = Key4A[1:12, ];Key4B = Key4B[1:12, ]

#drop unused columns
Key1A = Key1A[ , -c(1:4, 6, 8, 10:16)]
Key1A = Key1A[order(Key1A$Stimuli.Value),]

Key1B = Key1B[ , -c(1:4, 6, 8, 10:16)]
Key1B = Key1B[order(Key1B$Stimuli.Value),]

Key2A = Key2A[ , -c(1:4, 6, 8, 10:16)]
Key2A = Key2A[order(Key2A$Stimuli.Value),]

Key2B = Key2B[ , -c(1:4, 6, 8, 10:16)]
Key2B = Key2B[order(Key2B$Stimuli.Value),]

Key3A = Key3A[ , -c(1:4, 6, 8, 10:16)]
Key3A = Key3A[order(Key3A$Stimuli.Value),]

Key3B = Key3B[ , -c(1:4, 6, 8, 10:16)]
Key3B = Key3B[order(Key3B$Stimuli.Value),]

Key4A = Key4A[ , -c(1:4, 6, 8, 10:16)]
Key4A = Key4A[order(Key4A$Stimuli.Value),]

Key4B = Key4B[ , -c(1:4, 6, 8, 10:16)]
Key4B = Key4B[order(Key4B$Stimuli.Value),]

##Write keys to file
#write.csv(Key1A, file = "Key1A.csv", row.names = F)
#write.csv(Key1B, file = "Key1B.csv", row.names = F)

#write.csv(Key2A, file = "Key2A.csv", row.names = F)
#write.csv(Key2B, file = "Key2B.csv", row.names = F)

#write.csv(Key3A, file = "Key3A.csv", row.names = F)
#write.csv(Key3B, file = "Key3B.csv", row.names = F)

#write.csv(Key4A, file = "Key4A.csv", row.names = F)
#write.csv(Key4B, file = "Key4B.csv", row.names = F)

##Write merged data to file
#write.csv(dat, file = "item.csv", row.names = F)
#write.csv(dat2, file = "global.csv", row.names = F)
#write.csv(dat3, file = "Read.csv", row.names = F)
