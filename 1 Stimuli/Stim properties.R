####Read in the stim####
dat = read.csv("Ex 1 stim.csv")

library(ez)
options(scipen = 999)

#subset counterbalances
A = subset(dat,
           dat$Counterbalance == "A")
B = subset(dat,
           dat$Counterbalance == "B")

####Means####
##okay, do lists in general differ on anything?
#con
tapply(A$Con, list(A$Type, A$List.Num), mean, na.rm = T)
tapply(B$Con, list(B$Type, B$List.Num), mean, na.rm = T)

tapply(A$Con, list(A$Type, A$List.Num), sd, na.rm = T)
tapply(B$Con, list(B$Type, B$List.Num), sd, na.rm = T)

#freq
tapply(A$Freq, list(A$Type, A$List.Num), mean, na.rm = T)
tapply(B$Freq, list(B$Type, B$List.Num), mean, na.rm = T)

#length
tapply(A$Length, list(A$Type, A$List.Num), mean, na.rm = T)
tapply(B$Length, list(B$Type, B$List.Num), mean, na.rm = T)

####t-tests####
A.cat = subset(A, A$Type == "C")
B.cat = subset(B, B$Type == "C")

A.un = subset(A, A$Type == "U")
B.un = subset(B, B$Type == "U")

###Between counterbalances
##CON
t.test(A.cat$Con, B.cat$Con, paired = F, var.equal = T) #NS #p = .27
t.test(A.un$Con, B.un$Con, paired = F, var.equal = T) #NS #p = .39

#freq
t.test(A.cat$Freq, B.cat$Freq, paired = F, var.equal = T) #NS #p = .72
t.test(A.un$Freq, B.un$Freq, paired = F, var.equal = T) #NS #p = .75

#length
t.test(A.cat$Length, B.cat$Length, paired = F, var.equal = T) #NS #p = .36
t.test(A.un$Length, B.un$Length, paired = F, var.equal = T) #NS #p = .14

##between list types
#CON
t.test(A.cat$Con, A.un$Con, paired = F, var.equal = T) #SIG #p = .006 #uncategorized lists less concrete
t.test(B.cat$Con, B.un$Con, paired = F, var.equal = T) #SIG #p = .02

#freq
t.test(A.cat$Freq, A.un$Freq, paired = F, var.equal = T) # .86
t.test(B.cat$Freq, B.un$Freq, paired = F, var.equal = T) # .42

#length
t.test(A.cat$Length, A.un$Length, paired = F, var.equal = T) # .92
t.test(B.cat$Length, B.un$Length, paired = F, var.equal = T) # .02

####Get values for Table A1####
#Con
mean(A.cat$Con, na.rm = T); sd(A.cat$Con, na.rm = T)
mean(B.cat$Con, na.rm = T); sd(B.cat$Con, na.rm = T)

mean(A.un$Con, na.rm = T); sd(A.un$Con, na.rm = T)
mean(B.un$Con, na.rm = T); sd(B.un$Con, na.rm = T)

#Freq
mean(A.cat$Freq, na.rm = T); sd(A.cat$Freq, na.rm = T)
mean(B.cat$Freq, na.rm = T); sd(B.cat$Freq, na.rm = T)

mean(A.un$Freq, na.rm = T); sd(A.un$Freq, na.rm = T)
mean(B.un$Freq, na.rm = T); sd(B.un$Freq, na.rm = T)

#Length
mean(A.cat$Length, na.rm = T); sd(A.cat$Length, na.rm = T)
mean(B.cat$Length, na.rm = T); sd(B.cat$Length, na.rm = T)

mean(A.un$Length, na.rm = T); sd(A.un$Length, na.rm = T)
mean(B.un$Length, na.rm = T); sd(B.un$Length, na.rm = T)

#Length
ezANOVA(dat,
        wid = Word,
        dv = Length,
        between = .(Counterbalance, Type),
        type = 3,
        detailed = T) #(1, 92) = 2.71; p = .10

dat2 = na.omit(dat)

#freq
ezANOVA(dat2,
        wid = Word,
        dv = Freq,
        between = .(Counterbalance, Type),
        type = 3,
        detailed = T) #ns (1, 77) < 1, p = .62

#con
ezANOVA(dat2,
        wid = Word,
        dv = Con,
        between = .(Counterbalance, Type),
        type = 3,
        detailed = T) #ns (1, 77) = 1.70, p = .20

####Ex 2####
dat = read.csv("Ex 2 Stim.csv")

dat$Con = as.numeric(dat$Con)

tapply(dat$BAS, dat$Counterbalance, mean)
tapply(dat$BAS, dat$Counterbalance, sd)

tapply(dat$Length, dat$Counterbalance, mean)
tapply(dat$Length, dat$Counterbalance, sd)

tapply(dat$Con, dat$Counterbalance, mean, na.rm = T)
tapply(dat$Con, dat$Counterbalance, sd, na.rm = T)

tapply(dat$Freq, dat$Counterbalance, mean)
tapply(dat$Freq, dat$Counterbalance, sd)
