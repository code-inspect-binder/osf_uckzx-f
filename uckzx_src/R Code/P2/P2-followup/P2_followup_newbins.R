#HEADS & TAILS: Informational value of Greek coinage

#Prediction 2 followup: Do motifs become organised according to denomination?

#DESIGN-BASED ANALYSIS (OM)

#____________________________________________________________#
#SETTING UP
rm(list = ls())

#Reading in the data.
df <- read.csv2("2018_01_10_CoinImagesAll.csv", header = TRUE, strip.white = TRUE, 
                na.strings=c("","NA"), stringsAsFactors = F)

#Installing the package.
require(infotheo)

#____________________________________________________________#
#CHRONOLOGICAL ANALYSIS#

#DATE = 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)
#motifs = motifs on both sides of the coin which form designs of one coin type
#authorities = issuing authority of the coin (city, mint or ruler)

#Getting the list of unique dates
dates <- sort(unique(df$DATE))
print(dates)

#Creating output vectors for the loop
DATE <- c()
HDenomination <- c() #Entropy of denomination
HMotifs <- c() #Entropy of motifs
CEMotifsDenominations <- c() #Conditional entropy of motifs given denominations
NormCEMotifsDenominations <- c() #normalized conditional entropy of motifs given denominations

#Creating subsets of coins by time period and obtaining measures for each of the time periods
for (i in dates) { 
  dsub <- (subset(df, df$DATE == i))
  motifs <- cbind(dsub[318:681])
  denom <- cbind(dsub[256:309])  
  DATE[i] <- i
  HDenomination[i] <- entropy(denom)
  HMotifs[i] <- entropy(motifs)
  CEMotifsDenominations[i] <- condentropy(motifs, denom)
  NormCEMotifsDenominations[i] <- condentropy(motifs, denom) / entropy(motifs)
}

#Combining the vectors into a results data frame
results = as.data.frame(cbind(DATE, HDenomination, HMotifs, CEMotifsDenominations, NormCEMotifsDenominations))
results <- na.omit(as.data.frame(results))

write.csv2(results, "P2_followup_newbins_results.csv")

#____________________________________________________________#
#STATISTICAL TESTING: non-parametric Spearman's correlation

rdates <- rev(dates) #dates = years BCE

cor.test(results$DATE, results$HMotifs, method = "spearman")

plot(results$DATE, results$HMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(d|D)", main = "P2: Entropy of designs given denominations")
axis(1, at = rdates, labels = rdates)

#non-normalized conditional entropy
cor.test(results$DATE, results$CEMotifsDenominations, method = "spearman") 

plot(results$DATE, results$CEMotifsDenominations, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(d|D)", main = "P2: Conditional entropy of designs given denominations")
axis(1, at = rdates, labels = rdates)

#normalized conditional entropy
cor.test(results$DATE, results$NormCEMotifsDenominations, method = "spearman") 

plot(results$DATE, results$NormCEMotifsDenominations, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(d|D) / H(d)", main = "P2: Normalized conditional entropy of designs given denominations")
axis(1, at = rdates, labels = rdates)


#____________________________________________________________#
#ANALYSIS BY AUTHORITIES & NESTED REGRESSION

#Creating output vectors for the loop
DATE <- c()
HDenomination <- c() #Entropy of denomination
HMotifs <- c() #Entropy of motifs
CEMotifsDenominations <- c() #Conditional entropy of motifs given denominations
NormCEMotifsDenominations <- c() #normalized conditional entropy of motifs given denominations


#Creating subsets of coins by time period, obtaining measures for each authority inside of each time period
dates <- sort(unique(df$DATE))
for (i in dates){
  datesub <- subset(df, df$DATE == i)
  authorities <- sort(unique(datesub$AUTHORITY))
  for (j in authorities){
    motifs <- subset(datesub[,318:681], datesub$AUTHORITY == j)
    denom <- subset(datesub[,256:309], datesub$AUTHORITY == j)
    DATE[j] <- i
    HDenomination[j] <- entropy(denom)
    HMotifs[j] <- entropy(motifs)
    CEMotifsDenominations[j] <- condentropy(motifs, denom)
    NormCEMotifsDenominations[j] <- condentropy(motifs, denom) / entropy(motifs)
  }
  assign(paste0("results",i),cbind(DATE, HDenomination, HMotifs, CEMotifsDenominations, NormCEMotifsDenominations))
}

#Combining the results files
resultspoleis <- as.data.frame(rbind(results330,results350,results370,results390,results405,results415,results425,
                                     results435,results455,results470,results490,results600))
write.csv2(resultspoleis, "P2_followup_newbins_results_bypolis.csv")

#____________________________________________________________#
#NESTED REGRESSION
#dependent variable = normalized conditional entropy of denominations given designs
#predictor = normalised date


#non-normalized CE

resultspoleis$AUTHORITIES <- rownames(resultspoleis)
resultspoleis$DATE <- as.numeric(as.character(resultspoleis$DATE))

require(lme4)
test0 <- lmer(CEMotifsDenominations ~ (1|AUTHORITIES), data = resultspoleis)
testS <- lmer(CEMotifsDenominations ~ (1|AUTHORITIES) + (1 + DATE|AUTHORITIES), data = resultspoleis)
testD <- lmer(CEMotifsDenominations ~ (1|AUTHORITIES) + DATE, data = resultspoleis)

summary(test0)
summary(testS)
summary(testD)

anova(testD,test0)


#normalized CE

require(lme4)
test0 <- lmer(NormCEMotifsDenominations ~ (1|AUTHORITIES), data = resultspoleis)
testS <- lmer(NormCEMotifsDenominations ~ (1|AUTHORITIES) + (1 + DATE|AUTHORITIES), data = resultspoleis)
testD <- lmer(NormCEMotifsDenominations ~ (1|AUTHORITIES) + DATE, data = resultspoleis)

summary(test0)
summary(testS)
summary(testD)

anova(testD,test0)

