#HEADS & TAILS: Informational value of Greek coinage

#Prediction 3: Motifs are more informative about lower denominations than about higher ones.

#ANALYSIS ON DATA SUBSETS (BP)

#____________________________________________________________#
#SETTING UP
rm(list = ls())

#Reading in the data.
df <- read.csv2("2018_01_10_CoinImagesAll.csv", header = TRUE, strip.white = TRUE, 
                na.strings=c("","NA"), stringsAsFactors = F)

#Installing the package.
require(infotheo)

#____________________________________________________________#
#Calculating the conditional entropy of denominations given designs and conditional entropy of 
#authorities given desings separately for each df - HIGHER/LOWER

require(infotheo)

#Splitting the dataset at the base denomination value - HIGHER/LOWER denominations
values <- sort(unique(df$RELATIONAL.VALUE))
higher <- subset(df, df$RELATIONAL.VALUE >1.00)
lower <- subset(df, df$RELATIONAL.VALUE <=1.00)

#For HIGHER denominations
auth_higher <- as.data.frame(higher[,3:254])
denom_higher <- as.data.frame(higher[,256:309])
motifs_higher <- as.data.frame(higher[,318:681])

higher_CED <- condentropy(denom_higher, motifs_higher)
higher_CEA <- condentropy(auth_higher, motifs_higher)
higher_HM <- entropy(motifs_higher)
higher_HD <- entropy(denom_higher)
higher_HA <- entropy(auth_higher)

#For LOWER denominations
auth_lower <- as.data.frame(lower[,3:254])
denom_lower <- as.data.frame(lower[,256:309])
motifs_lower <- as.data.frame(lower[,318:681])

lower_CED <- condentropy(denom_lower, motifs_lower)
lower_CEA <- condentropy(auth_lower, motifs_lower)
lower_HM <- entropy(motifs_lower)
lower_HD <- entropy(denom_lower)
lower_HA <- entropy(auth_lower)

#Creating the results table
results_HL <- data.frame("CEDenomination.Designs"=numeric(2),"CEAuthority.Designs"=numeric(2), "Hmotifs"=numeric(2),
                         "HDenominations" = numeric(2),"HAuthorities"=numeric(2),row.names = c("HIGH","LOW"))
results_HL$CEDenomination.Designs <- c(higher_CED,lower_CED)
results_HL$CEAuthority.Designs <- c(higher_CEA,lower_CEA)
results_HL$Hmotifs <- c(higher_HM,lower_HM)
results_HL$HDenominations <- c(higher_HD,lower_HD)
results_HL$HAuthorities <- c(higher_HA,lower_HA)

results_HL

#____________________________________________________________#
#Calculating the conditional entropy of denomination given designs by authorities 
#separately for each df - HIGHER/LOWER

#Higher denominations
authority <- sort(unique(higher$AUTHORITY))
CEDenomination.Designs <- c()
HDenominations <- c()
NormCEDenomination.Designs <- c()
Ncoins <- c()
AUTHORITY <- c()
for(i in authority){
  high_sub <- subset(higher, higher$AUTHORITY == i)
  denom_high <- as.data.frame(high_sub[,256:309])
  motifs_high <- as.data.frame(high_sub[,318:681])
  AUTHORITY[i] <- i
  CEDenomination.Designs[i] <- condentropy(denom_high, motifs_high)
  HDenominations[i] <- entropy(denom_high)
  NormCEDenomination.Designs[i] <- condentropy(denom_high, motifs_high) / entropy(denom_high)
  Ncoins[i] <- nrow(high_sub)
}
results_higher <- data.frame("AUTHORITY" = AUTHORITY, "CEDenomination.Designs" = CEDenomination.Designs,
                             "Entropy.Denominations" = HDenominations,
                             "NormCEDenomination.Designs" = NormCEDenomination.Designs, "Ncoins" = Ncoins)
write.csv2(results_higher,"P3test_byauth_higher.csv")

#Lower denominations
authority <- sort(unique(lower$AUTHORITY))
CEDenomination.Designs <- c()
HDenominations <- c()
NormCEDenomination.Designs <- c()
Ncoins <- c()
AUTHORITY <- c()
for(i in authority){
  low_sub <- subset(lower, lower$AUTHORITY == i)
  denom_low <- as.data.frame(low_sub[,256:309])
  motifs_low <- as.data.frame(low_sub[,318:681])
  AUTHORITY[i] <- i
  CEDenomination.Designs[i] <- condentropy(denom_low, motifs_low)
  HDenominations[i] <- entropy(denom_low)
  NormCEDenomination.Designs[i] <- condentropy(denom_low, motifs_low) / entropy(denom_low)
  Ncoins[i] <- nrow(low_sub)
}
results_lower <- data.frame("AUTHORITY" = AUTHORITY, "CEDenomination.Designs" = CEDenomination.Designs,
                            "Entropy.Denominations" = HDenominations,
                            "NormCEDenomination.Designs" = NormCEDenomination.Designs, "Ncoins" = Ncoins)
write.csv2(results_lower,"P3test_byauth_lower.csv")

#Wilcoxon test: remove CE values for subset which contain only 1 coin type
rh1 <- subset(results_higher, results_higher$Ncoins != 1)
h1 <- rh1$CEDenomination.Designs
rl1 <- subset(results_lower, results_lower$Ncoins != 1)
l1 <- rl1$CEDenomination.Designs
rdata <- data.frame("CED" = c(h1,l1), "SUBSET"=c(rep("higher denomination", times=length(h1)),
                                                   rep("lower denomination",times=length(l1))))

wilcox.test(CED ~ SUBSET, data = rdata)
#W = number of times the CED by authority of lower denom is smaller than NormCED by authority of higher denom

#Plotting the higher vs. lower CED by authorities; subsets with only 1 coin are excluded
require(ggplot2)
rCEDauth <- as.data.frame(c(rh1$CEDenomination.Designs, rl1$CEDenomination.Designs))
colnames(rCEDauth) <- "CED"
rCEDauth$authority <- c(rh1$AUTHORITY, rl1$AUTHORITY)
rCEDauth$subset <- c(rep("higher denomination", times=68),rep("lower denomination",times=114))
rCEDauth$ncoins <- c(rh1$Ncoins, rl1$Ncoins)

rCEDauth$authority <- as.factor(rCEDauth$authority)
ggplot(rCEDauth) + geom_point(aes(x = authority, y = CED, colour = subset, size = ncoins)) +
  xlab("authority") + ylab("H(D|d)") +
  ggtitle("P3: Conditional entropy of denominations given designs by authorities for higher and lower denominations")

ggplot(rCEDauth, aes(x = authority, y = CED, fill = subset)) + 
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = .8), width = 0.7) + 
  xlab("authority") + ylab("H(D|d)") + geom_text(aes(x = authority, y = CED, label = ncoins),
    position = position_dodge(width = 1), vjust = -0.5, size = 2)

#____________________________________________________________#
#Calculating the conditional entropy of denomination given motif and conditional entropy of 
#authority given motif separately for each df- MORE/LESS

require(infotheo)

#Splitting the dataset at the median number of motifs per coin type - MORE/LESS motifs
df$NUM.OF.MOTIFS <- apply(df[,318:681], 1, sum)
median(df$NUM.OF.MOTIFS)
more <- subset(df, df$NUM.OF.MOTIFS >3)
less <- subset(df, df$NUM.OF.MOTIFS <=3)

#For MORE motifs
auth_more <- as.data.frame(more[,3:254])
denom_more <- as.data.frame(more[,256:309])
motifs_more <- as.data.frame(more[,318:681])

more_CED <- condentropy(denom_more, motifs_more)
more_CEA <- condentropy(auth_more, motifs_more)

#For LESS motifs
auth_less <- as.data.frame(less[,3:254])
denom_less <- as.data.frame(less[,256:309])
motifs_less <- as.data.frame(less[,318:681])

less_CED <- condentropy(denom_less, motifs_less)
less_CEA <- condentropy(auth_less, motifs_less)

#Creating the results table
results_ML <- data.frame("CEDenomination.Designs"=numeric(2),"CEAuthority.Designs"=numeric(2),
                         row.names = c("MORE","LESS"))
results_ML$CEDenomination.Designs <- c(more_CED,less_CED)
results_ML$CEAuthority.Designs <- c(more_CEA,less_CEA)

results_ML

#____________________________________________________________#
#Repeated test to establish the influence on N of motifs on H(D|d); analysis on coin types with 3 motifs
median(df$NUM.OF.MOTIFS) #median is 3

s <- subset(df,df$NUM.OF.MOTIFS == "3")

higher <- subset(s, s$RELATIONAL.VALUE >1.00)
lower <- subset(s, s$RELATIONAL.VALUE <=1.00)

require(infotheo)
#For HIGHER denominations
auth_higher <- as.data.frame(higher[,3:254])
denom_higher <- as.data.frame(higher[,256:309])
motifs_higher <- as.data.frame(higher[,318:681])

higher_CED <- condentropy(denom_higher, motifs_higher)
higher_CEA <- condentropy(auth_higher, motifs_higher) 


#For LOWER denominations
auth_lower <- as.data.frame(lower[,3:254])
denom_lower <- as.data.frame(lower[,256:309])
motifs_lower <- as.data.frame(lower[,318:681])

lower_CED <- condentropy(denom_lower, motifs_lower)
lower_CEA <- condentropy(auth_lower, motifs_lower)

#Creating the results table
results_hl3 <- data.frame("CEDenomination.Designs"=numeric(2),"CEAuthority.Designs"=numeric(2),
                         row.names = c("HIGH","LOW"))
results_hl3$CEDenomination.Designs <- c(higher_CED,lower_CED)
results_hl3$CEAuthority.Designs <- c(higher_CEA,lower_CEA)

results_hl3

#____________________________________________________________#
#H(D|d) for subsets by number of motifs on coins

#For HIGHER denominations
num <- sort(unique(df$NUM.OF.MOTIFS))
CEDenomination.Designs <- c()
NormCEDenomination.Designs <- c()
Ncoins <- c()
NUMofMotifs <- c()
for(i in num){
  s <- subset(df,df$NUM.OF.MOTIFS == i)
  high_sub <- subset(s, s$RELATIONAL.VALUE >1.00)
  denom_high <- as.data.frame(high_sub[,256:309])
  motifs_high <- as.data.frame(high_sub[,318:681])
  CEDenomination.Designs[i] <- condentropy(denom_high, motifs_high)
  NormCEDenomination.Designs[i] <- condentropy(denom_high, motifs_high) / entropy(denom_high)
  Ncoins[i] <- nrow(s)
  NUMofMotifs[i] <- i
}
results_higher_bynofMots <- data.frame("NUMofMotifs" = NUMofMotifs, "CEDenomination.Designs" = CEDenomination.Designs, 
                             "NormCEDenomination.Designs" = NormCEDenomination.Designs, "Ncoins"=Ncoins)
write.csv2(results_higher_bynofMots,"P3test_bynofMots_higher.csv")
results_higher_bynofMots


#For LOWER denominations
num <- sort(unique(df$NUM.OF.MOTIFS))
CEDenomination.Designs <- c()
NormCEDenomination.Designs <- c()
NUMofMotifs <- c()
Ncoins <- c()
for(i in num){
  s <- subset(df,df$NUM.OF.MOTIFS == i)
  low_sub <- subset(s, s$RELATIONAL.VALUE <=1.00)
  denom_low <- as.data.frame(low_sub[,256:309])
  motifs_low <- as.data.frame(low_sub[,318:681])
  CEDenomination.Designs[i] <- condentropy(denom_low, motifs_low)
  NormCEDenomination.Designs[i] <- condentropy(denom_low, motifs_low) / entropy(denom_low)
  NUMofMotifs[i] <- i
  Ncoins[i] <- nrow(s)
}
results_lower_bynofMots <- data.frame("NUMofMotifs" = NUMofMotifs, "CEDenomination.Designs" = CEDenomination.Designs, 
                                       "NormCEDenomination.Designs" = NormCEDenomination.Designs, "Ncoins"=Ncoins)
write.csv2(results_lower_bynofMots,"P3test_bynofMots_lower.csv")
results_lower_bynofMots


#Wilcoxon test: remove CE values for subset which contain only 1 coin type
rh2 <- subset(results_higher_bynofMots, results_higher_bynofMots$Ncoins != 1)
h2 <- rh2$CEDenomination.Designs
rl2 <- subset(results_lower_bynofMots, results_lower_bynofMots$Ncoins != 1)
l2 <- rl2$CEDenomination.Designs
rdata2 <- data.frame("CED" = c(h2,l2), "SUBSET"=c(rep("higher denomination", times=length(h2)),
                                               rep("lower denomination",times=length(l2))))
rdata2 <- na.omit(rdata2)
wilcox.test(CED ~ SUBSET, data = rdata2)
#W = number of times the CED by number of motifs on coin of lower denom is smaller than CED by authority of higher denom

#Plotting the higher vs. lower CED by number of motifs; subsets with only 1 coin are excluded
require(ggplot2)
rCEDmots <- as.data.frame(c(rh2$CEDenomination.Designs, rl2$CEDenomination.Designs))
colnames(rCEDmots) <- "CED"
rCEDmots$nmotifs <- c(rh2$NUMofMotifs, rl2$NUMofMotifs)
rCEDmots$subset <- c(rep("higher denomination", times=11),rep("lower denomination",times=11))
rCEDmots$ncoins <- c(rh2$Ncoins, rl2$Ncoins)

rCEDmots$nmotifs <- as.factor(rCEDmots$nmotifs)
ggplot(rCEDmots) + geom_point(aes(x = nmotifs, y = CED, colour = subset), size = 3) +
  geom_text(aes(x = nmotifs, y = CED, label=ncoins),hjust=0, vjust=0)+ xlab("number of motifs") + ylab("H(D|d)") +
  ggtitle("P3: Conditional entropy of denominations given designs by number\nof motifs on coin for higher and lower denominations")

ggplot(rCEDmots, aes(x = nmotifs, y = CED, fill = subset)) + 
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = .8), width = 0.7) + 
  xlab("number of motifs") + ylab("H(D|d)") + geom_text(aes(x = nmotifs, y = CED, label = ncoins),
  position = position_dodge(width = 1), vjust = -0.5, size = 2)


#____________________________________________________________#
#Full results table
results <- data.frame("CEDenomination.Designs"=numeric(4),"CEAuthority.Designs"=numeric(4),
                      row.names = c("MORE","LESS","HIGH","LOW"))

results$CEDenomination.Designs <- c(more_CED,less_CED,higher_CED,lower_CED)
results$CEAuthority.Designs <- c(more_CEA,less_CEA,higher_CEA,lower_CEA)

results

write.csv2(results,"P3_full.csv")
