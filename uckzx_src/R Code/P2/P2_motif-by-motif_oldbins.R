#HEADS & TAILS: Informational value of Greek coinage

#Prediction 2: Motifs become more informative about a coin's denomination.

#MOTIF-BASED ANALYSIS (BP)

#____________________________________________________________#
#SETTING UP
rm(list = ls())

#Reading in the data.
df <- read.csv2("2018_01_10_CoinImagesAll.csv", header = TRUE, strip.white = TRUE, 
                na.strings=c("","NA"), stringsAsFactors = F)

#Installing the package.
require(infotheo)

#____________________________________________________________#
#CHRONOLOGICAL ANALYSIS PER MOTIF

#DATE = preregistered 3 time periods (roughly 6th, 5th, 4th century BCE)
#motifs = motifs on both sides of the coin which form designs of one coin type
#authorities = issuing authority of the coin (city, mint or ruler)

#Creating output vectors for the loop
DATE <- c()
HDenomination <- c() #Entropy of denomination
HMotifs <- c() #Entropy of motifs
CEDenominationsMotifs <- c() #Conditional entropy of denominations given motifs
NormCEDenominationsMotifs <- c() #normalized conditional entropy of denominations given motifs
MI <- c() #Mutual information between denominations and motifs
CEMotifsDenominations <- c() #Conditional entropy of motifs given denominations
NormCEMotifsDenominations <- c() #normalized conditional entropy of motifs given denominations


#Creating subsets of coins by time period, obtaining measures for each motif inside of each time period
dates <- sort(unique(df$OLD.BINS))
for (i in dates){
  datesub <- (subset(df, df$OLD.BINS == i))
  motifs <- cbind(datesub[318:681])
  denom <- cbind(datesub[256:309])
  for (j in names(motifs)){
    motif <- as.data.frame(motifs[ ,which(colnames(motifs) == j)])
    colnames(motif) <- j
    DATE[j] <- i
    HDenomination[j] <- entropy(denom)
    HMotifs[j] <- entropy(motif)
    CEDenominationsMotifs[j] <- condentropy(denom, motif)
    NormCEDenominationsMotifs[j] <- condentropy(denom, motif) / entropy(denom)
    CEMotifsDenominations[j] <- condentropy(motif, denom)
    NormCEMotifsDenominations[j] <- condentropy(motif, denom) / entropy(motif)
    MI[j] <- mutinformation(denom, motif)
  }
  assign(paste0("results",i),cbind(DATE, HDenomination, HMotifs, CEDenominationsMotifs, NormCEDenominationsMotifs, 
                                   CEMotifsDenominations, NormCEMotifsDenominations, MI))
}

#Combining the results files
resultsmotifs <- as.data.frame(rbind(results4,results5,results6))
write.csv2(resultsmotifs, "P2analysis_oldbins_results_bymotif.csv")


#____________________________________________________________#
#Plots with H(D|M) = non-normalized conditional entropy

#Getting mean, median and standard deviation across different motifs per period
N <- aggregate(CEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = length)
MEAN <- aggregate(CEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = mean)
MEDIAN <- aggregate(CEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = median)
SD <- aggregate(CEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = sd)
resultsmotifs_summary <- cbind.data.frame(N, MEAN$CEDenominationsMotifs, MEDIAN$CEDenominationsMotifs, 
                                          SD$CEDenominationsMotifs)
colnames(resultsmotifs_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
resultsmotifs_summary$SE <- resultsmotifs_summary$SD / sqrt(resultsmotifs_summary$N)

#Plot mean and median H(D|M) per motif per period
require(ggplot2)
ggmean <- ggplot(resultsmotifs_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P2: Mean conditional entropy of denominations given motifs across motifs", 
  x = "Century BCE", y = "mean H(D|M) across motifs") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=resultsmotifs_summary$MEAN-resultsmotifs_summary$SE, 
  ymax=resultsmotifs_summary$MEAN+resultsmotifs_summary$SE),width=.1) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(resultsmotifs_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P2: Median conditional entropy of denominations given motifs across motifs", 
       x = "Century BCE", y = "median H(D|M) across motifs") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(D|M) per motifs per date
rdates <- rev(dates)
plot(resultsmotifs$DATE, resultsmotifs$CEDenominationsMotifs, xlim = c(6,4), xaxt='n', xlab = "Century BCE", 
     ylab = "H(D|M) per motifs", main = "P2: conditional entropy of denominations given motifs for each motif")
axis(1, at = rdates, labels = rdates)

rdates <- rev(dates) #dates = century BCE
resultsmotifs$DATE <- as.factor(resultsmotifs$DATE)

p1 <- ggplot(resultsmotifs, aes(x=DATE, y=CEDenominationsMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P2: conditional entropy of denominations given motifs for each motif") +
  labs(y= "H(D|M) per motifs", x = "Century BCE")
p1

p2 <- ggplot(resultsmotifs, aes(x=DATE, y=CEDenominationsMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P2: conditional entropy of denominations given motifs for each motif") +
  labs(y= "H(D|M) per motifs", x = "Century BCE")
p2

#____________________________________________________________#
#Plots with H(D|M)/H(D) - normalized conditional entropy

resultsmotifs <- as.data.frame(rbind(results4,results5,results6))

#Getting mean, median and standard deviation across different motifs per period
N <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = length)
MEAN <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = mean)
MEDIAN <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = median)
SD <- aggregate(NormCEDenominationsMotifs ~ DATE, data = resultsmotifs, FUN = sd)
Nresultsmotifs_summary <- cbind.data.frame(N, MEAN$NormCEDenominationsMotifs, MEDIAN$NormCEDenominationsMotifs, 
                                          SD$NormCEDenominationsMotifs)
colnames(Nresultsmotifs_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
Nresultsmotifs_summary$SE <- Nresultsmotifs_summary$SD / sqrt(Nresultsmotifs_summary$N)

#Plot mean and median H(D|M)/H(D) per motif per period
require(ggplot2)
ggmean <- ggplot(Nresultsmotifs_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P2: Mean normalized conditional entropy of denominations given motifs across motifs", 
  x = "Century BCE", y = "mean H(D|M)/H(D) across motifs") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=Nresultsmotifs_summary$MEAN-Nresultsmotifs_summary$SE, 
  ymax=Nresultsmotifs_summary$MEAN+Nresultsmotifs_summary$SE),width=.1) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(Nresultsmotifs_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P2: Median normalized conditional entropy of denominations given motifs across motifs", 
       x = "Century BCE", y = "median H(D|M)/H(D) across motifs") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(D|M)/H(D) per motifs per date
rdates <- rev(dates)
plot(resultsmotifs$DATE, resultsmotifs$NormCEDenominationsMotifs, xlim = c(6,4), xaxt='n', xlab = "Century BCE", 
     ylab = "H(D|M)/H(D) per motifs", main = "P2: normalized conditional entropy of denominations given motifs for 
     each motif")
axis(1, at = rdates, labels = rdates)

require(ggplot2)
resultsmotifs$DATE <- as.factor(resultsmotifs$DATE)

p1 <- ggplot(resultsmotifs, aes(x=DATE, y=NormCEDenominationsMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P2: normalized conditional entropy of denominations given motifs for each motif") +
  labs(y= "H(D|M)/H(D) per motifs", x = "Century BCE")
p1

p2 <- ggplot(resultsmotifs, aes(x=DATE, y=NormCEDenominationsMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P2: normalized conditional entropy of denominations given motifs for each motif") +
  labs(y= "H(D|M)/H(D) per motifs", x = "Century BCE")
p2


#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY MOTIFS - non-normalized CE

resultsmotifs$MOTIFS <- rownames(resultsmotifs)
resultsmotifs$DATE <- as.numeric(as.character(resultsmotifs$DATE))
#dependent variable = non-normalized conditional entropy of denominations given motifs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(CEDenominationsMotifs ~ (1|MOTIFS), data = resultsmotifs)
testD <- lmer(CEDenominationsMotifs ~ (1|MOTIFS) + DATE, data = resultsmotifs)

summary(test0)
summary(testD)

anova(test0,testD)

#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY MOTIFS - normalized CE

resultsmotifs$MOTIFS <- rownames(resultsmotifs)
resultsmotifs$DATE <- as.numeric(as.character(resultsmotifs$DATE))
#dependent variable = normalized conditional entropy of denominations given motifs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(NormCEDenominationsMotifs ~ (1|MOTIFS), data = resultsmotifs)
testD <- lmer(NormCEDenominationsMotifs ~ (1|MOTIFS) + DATE, data = resultsmotifs)

summary(test0)
summary(testD)

anova(test0,testD)

