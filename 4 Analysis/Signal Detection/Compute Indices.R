####Set up####
##load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##read in data
ex1 = read.csv("Data/sd ex1b.csv")

####Get the data in the right shape####
##For each participant, it looks like I need to sum total for each of the four categories
##Hits, False alarms, misses, and correct rejections

##Make the columns
ex1$control = ex1$Direction
ex1$control[ex1$control == "Related"] = "Presented"
ex1$control[ex1$control == "Unrelated"] = "Presented"

##first, code each trial as being one of the categories
#not presented, 0 == correct rejection
#not presented, 1 == False alarm
#presented, 0 == miss
#presented, 1 == hit

###First, Experiment 1
##Start w/ hits
hits1 = subset(ex1,
               ex1$control == "Presented" & ex1$scored == 1)
hits1$sig_type = rep("hit")

miss1 = subset(ex1,
             ex1$control == "Presented" & ex1$scored == 0)
miss1$sig_type = rep("miss")

fa1 = subset(ex1,
             ex1$control == "Control" & ex1$scored == 1)
fa1$sig_type = rep("fa")

cr1 = subset(ex1,
             ex1$control == "Control" & ex1$scored == 0)
cr1$sig_type = rep("cr")

##put it all back together
combined1 = rbind(hits1, miss1, fa1, cr1)

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()

for(i in unique(combined1$Username)){
  
  #loop through participants
  temp = subset(combined1, 
                combined1$Username == i)
  
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$Encoding[1]
  
  #now slap it back together
  temp2 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp2)
  
}

####compute indices####
indices = data.frame(dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c))
ex1_final = cbind(sig_detect, indices)

####Now for Experiment 2####
ex2 = read.csv("Data/sd ex2b.csv")
table(ex2$Item_type)

##first, code each trial as being one of the categories
#List Item Control, 0 == correct rejection
#List Item Control, 1 == False alarm
#List Item, 0 == miss
#List Item, 1 == hit

##Start w/ presented and non-presented items
hits2 = subset(ex2,
               ex2$Item_type == "List Item" & ex2$scored == 1)
hits2$sig_type = rep("hit")

miss2 = subset(ex2,
               ex2$Item_type == "List Item" & ex2$scored == 0)
miss2$sig_type = rep("miss")

fa2 = subset(ex2,
             ex2$Item_type == "List Item Control" & ex2$scored == 1)
fa2$sig_type = rep("fa")

cr2 = subset(ex2,
             ex2$Item_type == "List Item Control" & ex2$scored == 0)
cr2$sig_type = rep("cr")

##Now DRM items
#Critical Lures
hits_cl = subset(ex2,
             ex2$Item_type == "Critical Lure" & ex2$scored == 1)
hits_cl$sig_type = rep("hit")

miss_cl = subset(ex2,
             ex2$Item_type == "Critical Lure" & ex2$scored == 0)
miss_cl$sig_type = rep("miss")

#Critical lure controls
fa2_clc = subset(ex2,
                ex2$Item_type == "Critical Lure Control" & ex2$scored == 1)
fa2_clc$sig_type = rep("fa_clc")

cr2_clc = subset(ex2,
                ex2$Item_type == "Critical Lure Control" & ex2$scored == 0)
cr2_clc$sig_type = rep("cr_clc")

##put it all back together
combined2 = rbind(hits2, miss2, fa2, cr2)

####Now sum the categories for each participant####
###Presented
sig_detect = data.frame()

for(i in unique(combined2$Username)){
  
  #loop through participants
  temp = subset(combined2, 
                combined2$Username == i)
  
  ##Presented items
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  ##non presented items
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$encoding[1]
  
  #now slap it back together
  temp3 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp3)
  
}

####compute indices####
indices = dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c)
ex2_presented = cbind(sig_detect, indices)

##False Memory
#Treat critical lures as correct memory
combined3 = rbind(hits_cl, miss_cl, fa2_clc, cr2_clc)

sig_detect = data.frame()

for(i in unique(combined3$Username)){
  
  #loop through participants
  temp = subset(combined3, 
                combined3$Username == i)
  
  ##Presented items
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  ##non presented items
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa_clc")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr_clc")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$encoding[1]
  
  #now slap it back together
  temp3 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp3)
  
}

####compute indices####
indices = dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c)
ex2_false = cbind(sig_detect, indices)

tapply(ex2_false$dprime, ex2_false$e, mean)

####Write everything to .csv for analyses####
#write.csv(ex1_final, file = "Sig Data/ex1_sig.csv", row.names = F)
#write.csv(ex2_presented, file = "Sig Data/ex2_sig_pres.csv", row.names = F)
#write.csv(ex2_false, file = "Sig Data/ex2_sig_false.csv", row.names = F)
