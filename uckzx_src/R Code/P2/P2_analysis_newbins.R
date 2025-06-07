#HEADS & TAILS: Informational value of Greek coinage

#Prediction 2: Motifs become more informative about a coin's denomination.

#DESIGN-BASED ANALYSIS (OM & BP)

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

#Creating the list of time periods
dates <- sort(unique(df$DATE))
print(dates)

#Creating output vectors for the loop
DATE <- c()
HDenomination <- c() #Entropy of denomination
HMotifs <- c() #Entropy of motifs
CEDenominationsMotifs <- c() #Conditional entropy of denominations given designs
NormCEDenominationsMotifs <- c() #normalized conditional entropy of denominations given designs
MI <- c() #Mutual information between denominations and motifs
CEMotifsDenominations <- c() #Conditional entropy of motifs given denominations
NormCEMotifsDenominations <- c() #normalized conditional entropy of motifs given denominations
NDenominations <- c() #Number of unique denominations inside a period
NCoins <- c() #Number of coin types inside a period (number of observations)


#Creating subsets of coins by time period and obtaining measures for each of the time periods
for (i in dates) { 
  dsub <- (subset(df, df$DATE == i))
  motifs <- cbind(dsub[318:681])
  denom <- cbind(dsub[256:309])  
  DATE[i] <- i
  HDenomination[i] <- entropy(denom)
  HMotifs[i] <- entropy(motifs)
  CEDenominationsMotifs[i] <- condentropy(denom, motifs)
  NormCEDenominationsMotifs[i] <- condentropy(denom, motifs) / entropy(denom)
  CEMotifsDenominations[i] <- condentropy(motifs, denom)
  NormCEMotifsDenominations[i] <- condentropy(motifs, denom) / entropy(motifs)
  MI[i] <- mutinformation(denom, motifs)
  NDenominations[i] <- length(unique(dsub$DENOMINATION))
  NCoins[i] <- length(unique(dsub$ID))
}

#Combining the vectors into a results data frame
results = as.data.frame(cbind(DATE, HDenomination, HMotifs, CEDenominationsMotifs, NormCEDenominationsMotifs, 
                              CEMotifsDenominations, NormCEMotifsDenominations, MI, NDenominations, NCoins))
results <- na.omit(as.data.frame(results))

write.csv2(results, "P2analysis_newbins_results.csv")


#____________________________________________________________#
#STATISTICAL TESTING: non-parametric Spearman's correlation

#non-normalized conditional entropy
cor.test(results$DATE, results$CEDenominationsMotifs, method = "spearman") 

rdates <- rev(dates) #dates = years BCE
plot(results$DATE, results$CEDenominationsMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(D|d)", main = "P2: Conditional entropy of denomination given designs")
axis(1, at = rdates, labels = rdates)

#normalized conditional entropy
cor.test(results$DATE, results$NormCEDenominationsMotifs, method = "spearman") 

plot(results$DATE, results$NormCEDenominationsMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(D|d) / H(D)", main = "P2: normalized conditional entropy of denominations given designs")
axis(1, at = rdates, labels = rdates)


#____________________________________________________________#
#RE-ANALYSIS: GROUPING BY Authority, by period: fine-grained 13 time periods

#Creating output vectors for the loop
DATE <- c()
HDenomination <- c() #Entropy of denomination
HMotifs <- c() #Entropy of motifs
CEDenominationsMotifs <- c() #Conditional entropy of denominations given designs
NormCEDenominationsMotifs <- c() #normalized conditional entropy of denominations given designs
MI <- c() #Mutual information between denominations and motifs
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
    CEDenominationsMotifs[j] <- condentropy(denom, motifs)
    NormCEDenominationsMotifs[j] <- condentropy(denom, motifs) / entropy(denom)
    CEMotifsDenominations[j] <- condentropy(motifs, denom)
    NormCEMotifsDenominations[j] <- condentropy(motifs, denom) / entropy(motifs)
    MI[j] <- mutinformation(denom, motifs)
  }
  assign(paste0("results",i),cbind(DATE, HDenomination, HMotifs, CEDenominationsMotifs, NormCEDenominationsMotifs, 
                                   CEMotifsDenominations, NormCEMotifsDenominations, MI))
}

#Combining the results files
resultspoleis <- as.data.frame(rbind(results330,results350,results370,results390,results405,results415,results425,
                                     results435,results455,results470,results490,results600))
write.csv2(resultspoleis, "P2analysis_newbins_results_bypolis.csv")



#____________________________________________________________#
#Plots with H(D|d) = non-normalized conditional entropy

#Getting mean, median and standard deviation across different authorities per period for non-normalized conditional
#entropy of denomination given designs
N <- aggregate(CEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = length)
MEAN <- aggregate(CEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = mean)
MEDIAN <- aggregate(CEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = median)
SD <- aggregate(CEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = sd)
resultspoleis_summary <- cbind.data.frame(N, MEAN$CEDenominationsMotifs, MEDIAN$CEDenominationsMotifs, 
                                          SD$CEDenominationsMotifs)
colnames(resultspoleis_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
resultspoleis_summary$SE <- resultspoleis_summary$SD / sqrt(resultspoleis_summary$N)

#Plot mean and median H(D|d) across authorities per period
require(ggplot2)
ggmean <- ggplot(resultspoleis_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P2: Mean conditional entropy of denominations given designs across authorities", 
  x = "Year BCE", y = "mean H(D|d) across authorities") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=resultspoleis_summary$MEAN-resultspoleis_summary$SE, 
  ymax=resultspoleis_summary$MEAN+resultspoleis_summary$SE),width=.1) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(resultspoleis_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P2: Median conditional entropy of denominations given designs across authorities", 
  x = "Year BCE", y = "median H(D|d) across authorities") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(D|d) per authorities per date
rdates <- rev(dates)
plot(resultspoleis$DATE, resultspoleis$CEDenominationsMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(D|d) per authorities", main = "P2: Conditional entropy of denominations given designs for each authority")
axis(1, at = rdates, labels = rdates)

require(ggplot2)
resultspoleis$DATE <- as.factor(resultspoleis$DATE)

p1 <- ggplot(resultspoleis, aes(x=DATE, y=CEDenominationsMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultspoleis$DATE))) +
  ggtitle("P2: Conditional entropy of denominations given designs for each authority") +
  labs(y= "H(D|d) per authorities", x = "Year BCE")
p1

p2 <- ggplot(resultspoleis, aes(x=DATE, y=CEDenominationsMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultspoleis$DATE))) +
  ggtitle("P2: Conditional entropy of denominations given designs for each authority") +
  labs(y= "H(D|d) per authorities", x = "Year BCE")
p2

#____________________________________________________________#
#Plots with H(D|d)/H(D) = normalized conditional entropy

resultspoleis <- as.data.frame(rbind(results330,results350,results370,results390,results405,results415,results425,
                                     results435,results455,results470,results490,results600))

#Getting mean, median and standard deviation across different authorities per period for normalized conditional
#entropy of denomination given designs
N <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = length)
MEAN <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = mean)
MEDIAN <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = median)
SD <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultspoleis, FUN = sd)
Nresultspoleis_summary <- cbind.data.frame(N, MEAN$NormCEDenominationsMotifs, MEDIAN$NormCEDenominationsMotifs, 
                                          SD$NormCEDenominationsMotifs)
colnames(Nresultspoleis_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
Nresultspoleis_summary$SE <- Nresultspoleis_summary$SD / sqrt(Nresultspoleis_summary$N)

#Plot mean and median H(D|d)/H(D) across authorities per period
require(ggplot2)
ggmean <- ggplot(Nresultspoleis_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P2: Mean normalized conditional entropy of denominations given designs \nacross authorities", 
       x = "Year BCE", y = "mean H(D|d)/H(D) across authorities") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=Nresultspoleis_summary$MEAN-Nresultspoleis_summary$SE, 
                                        ymax=Nresultspoleis_summary$MEAN+Nresultspoleis_summary$SE),width=5) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(Nresultspoleis_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P2: Median normalized conditional entropy of denominations given designs \nacross authorities", 
       x = "Year BCE", y = "median H(D|d)/H(D) across authorities") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(D|d)/H(D) per authorities per date
rdates <- rev(dates)
plot(resultspoleis$DATE, resultspoleis$NormCEDenominationsMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(D|d/H(D) per authorities", main = "P2: Normalized conditional entropy of denominations \ngiven designs for each authority")
axis(1, at = rdates, labels = rdates)

require(ggplot2)
resultspoleis$DATE <- as.factor(resultspoleis$DATE)

p1 <- ggplot(resultspoleis, aes(x=DATE, y=NormCEDenominationsMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultspoleis$DATE))) +
  ggtitle("P2: Normalized conditional entropy of denominations \ngiven designs for each authority") +
  labs(y= "H(D|d)/H(D) per authorities", x = "Year BCE")
p1

p2 <- ggplot(resultspoleis, aes(x=DATE, y=NormCEDenominationsMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultspoleis$DATE))) +
  ggtitle("P2: Normalized conditional entropy of denominations \ngiven designs for each authority") +
  labs(y= "H(D|d)/H(D) per authorities", x = "Year BCE")
p2

#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY AUTHORITIES - non-normalized CE

resultspoleis$AUTHORITIES <- rownames(resultspoleis)
resultspoleis$DATE <- as.numeric(as.character(resultspoleis$DATE))

#dependent variable = non-normalized conditional entropy of denominations given designs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(CEDenominationsMotifs ~ (1|AUTHORITIES), data = resultspoleis)
testD <- lmer(CEDenominationsMotifs ~ (1|AUTHORITIES) + DATE, data = resultspoleis)

summary(test0)
summary(testD)

anova(test0,testD)

#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY AUTHORITIES - normalized CE

resultspoleis$AUTHORITIES <- rownames(resultspoleis)
resultspoleis$DATE <- as.numeric(as.character(resultspoleis$DATE))

#dependent variable = normalized conditional entropy of denominations given designs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(NormCEDenominationsMotifs ~ (1|AUTHORITIES), data = resultspoleis)
testD <- lmer(NormCEDenominationsMotifs ~ (1|AUTHORITIES) + DATE, data = resultspoleis)

summary(test0)
summary(testD)

anova(test0,testD)
