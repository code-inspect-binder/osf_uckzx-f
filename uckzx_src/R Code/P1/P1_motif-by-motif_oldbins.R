#HEADS & TAILS: Informational value of Greek coinage

#Prediction 1: Motifs become less informative about a coin's provenance.

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
HAuthority <- c() #Entropy of authority
HMotifs <- c() #Entropy of motifs
CEAuthoritiesMotifs <- c() #Conditional entropy of authority given motifs
NormCEAuthoritiesMotifs <- c() #normalized conditional entropy of authority given motifs
MI <- c() #Mutual information between authority and motifs
CEMotifsAuthorities <- c() #Conditional entropy of motifs given authority
NormCEMotifsAuthorities <- c() #normalized conditional entropy of motifs given authority


#Creating subsets of coins by time period, obtaining measures for each motif inside of each time period
dates <- sort(unique(df$OLD.BINS))
for (i in dates){
  datesub <- (subset(df, df$OLD.BINS == i))
  motifs <- cbind(datesub[318:681])
  autho <- cbind(datesub[3:254])
  for (j in names(motifs)){
    motif <- as.data.frame(motifs[ ,which(colnames(motifs) == j)])
    colnames(motif) <- j
    DATE[j] <- i
    HAuthority[j] <- entropy(autho)
    HMotifs[j] <- entropy(motif)
    CEAuthoritiesMotifs[j] <- condentropy(autho, motif)
    NormCEAuthoritiesMotifs[j] <- condentropy(autho, motif) / entropy(autho)
    CEMotifsAuthorities[j] <- condentropy(motif, autho)
    NormCEMotifsAuthorities[j] <- condentropy(motif, autho) / entropy(motif)
    MI[j] <- mutinformation(autho, motif)
  }
  assign(paste0("results",i),cbind(DATE, HAuthority, HMotifs, CEAuthoritiesMotifs, NormCEAuthoritiesMotifs, 
                                   CEMotifsAuthorities, NormCEMotifsAuthorities, MI))
}

#Combining the results files
resultsmotifs <- as.data.frame(rbind(results4,results5,results6))
write.csv2(resultsmotifs, "P1analysis_oldbins_results_bymotif.csv")


#____________________________________________________________#
#Plots with H(A|M) = non-normalized conditional entropy

#Getting mean, median and standard deviation across different motifs per period
N <- aggregate(CEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = length)
MEAN <- aggregate(CEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = mean)
MEDIAN <- aggregate(CEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = median)
SD <- aggregate(CEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = sd)
resultsmotifs_summary <- cbind.data.frame(N, MEAN$CEAuthoritiesMotifs, MEDIAN$CEAuthoritiesMotifs, 
                                          SD$CEAuthoritiesMotifs)
colnames(resultsmotifs_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
resultsmotifs_summary$SE <- resultsmotifs_summary$SD / sqrt(resultsmotifs_summary$N)

#Plot mean and median H(A|M) per motif per period
require(ggplot2)
ggmean <- ggplot(resultsmotifs_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P1: Mean conditional entropy of authorities given motifs across motifs", 
       x = "Century BCE", y = "mean H(A|M) across motifs") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=resultsmotifs_summary$MEAN-resultsmotifs_summary$SE, 
  ymax=resultsmotifs_summary$MEAN+resultsmotifs_summary$SE),width=.1) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(resultsmotifs_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P1: Median conditional entropy of authorities given motifs across motifs", 
       x = "Century BCE", y = "median H(A|M) across motifs") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(A|M) per motifs per date
rdates <- rev(dates)
plot(resultsmotifs$DATE, resultsmotifs$CEAuthoritiesMotifs, xlim = c(6,4), xaxt='n', xlab = "Century BCE", 
     ylab = "H(A|M) per motifs", main = "P1: Conditional entropy of authorities given motifs for each motif")
axis(1, at = rdates, labels = rdates)

rdates <- rev(dates) #dates = century BCE
resultsmotifs$DATE <- as.factor(resultsmotifs$DATE)

p1 <- ggplot(resultsmotifs, aes(x=DATE, y=CEAuthoritiesMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P1: Conditional entropy of authorities given motifs for each motif") +
  labs(y= "H(A|M) per motifs", x = "Century BCE")
p1

p2 <- ggplot(resultsmotifs, aes(x=DATE, y=CEAuthoritiesMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P1: Conditional entropy of authorities given motifs for each motif") +
  labs(y= "H(A|M) per motifs", x = "Century BCE")
p2

#____________________________________________________________#
#Plots with H(A|M)/H(A) - normalized conditional entropy

resultsmotifs <- as.data.frame(rbind(results4,results5,results6))

#Getting mean, median and standard deviation across different motifs per period
N <- aggregate(NormCEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = length)
MEAN <- aggregate(NormCEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = mean)
MEDIAN <- aggregate(NormCEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = median)
SD <- aggregate(NormCEAuthoritiesMotifs ~ DATE, data = resultsmotifs, FUN = sd)
Nresultsmotifs_summary <- cbind.data.frame(N, MEAN$NormCEAuthoritiesMotifs, MEDIAN$NormCEAuthoritiesMotifs, 
                                          SD$NormCEAuthoritiesMotifs)
colnames(Nresultsmotifs_summary) <- c("DATE","N","MEAN","MEDIAN","SD")
Nresultsmotifs_summary$SE <- Nresultsmotifs_summary$SD / sqrt(Nresultsmotifs_summary$N)

#Plot mean and median H(A|M)/H(A) per motif per period
require(ggplot2)
ggmean <- ggplot(Nresultsmotifs_summary,aes(x=DATE,y=MEAN)) + 
  labs(title =  "P1: Mean normalized conditional entropy of authorities given motifs across motifs", 
       x = "Century BCE", y = "mean H(A|M)/H(A) across motifs") + 
  scale_x_reverse() + geom_errorbar(aes(ymin=Nresultsmotifs_summary$MEAN-Nresultsmotifs_summary$SE, 
                                        ymax=Nresultsmotifs_summary$MEAN+Nresultsmotifs_summary$SE),width=.1) + 
  geom_line() + geom_point()
ggmean

ggmedian <- ggplot(Nresultsmotifs_summary,aes(x=DATE,y=MEDIAN)) + 
  labs(title =  "P1: Median normalized conditional entropy of authorities given motifs across motifs", 
       x = "Century BCE", y = "median H(A|M)/H(A) across motifs") +
  scale_x_reverse() + geom_line() + geom_point()
ggmedian

#Plot H(A|M) per motifs per date
rdates <- rev(dates)
plot(resultsmotifs$DATE, resultsmotifs$NormCEAuthoritiesMotifs, xlim = c(6,4), xaxt='n', xlab = "Century BCE", 
     ylab = "H(A|M)/H(A) per motifs", main = "P1: normalized conditional entropy of authorities given motifs for 
     each motif")
axis(1, at = rdates, labels = rdates)

require(ggplot2)
resultsmotifs$DATE <- as.factor(resultsmotifs$DATE)

p1 <- ggplot(resultsmotifs, aes(x=DATE, y=NormCEAuthoritiesMotifs)) + geom_violin(trim=T) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P1: normalized conditional entropy of authorities given motifs for each motif") +
  labs(y= "H(A|M)/H(A) per motifs", x = "Century BCE")
p1

p2 <- ggplot(resultsmotifs, aes(x=DATE, y=NormCEAuthoritiesMotifs)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1) +
  scale_x_discrete(limits = rev(levels(resultsmotifs$DATE))) +
  ggtitle("P1: normalized conditional entropy of authorities given motifs for each motif") +
  labs(y= "H(A|M)/H(A) per motifs", x = "Century BCE")
p2

#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY MOTIFS - performed on non-normalized values

resultsmotifs$MOTIFS <- rownames(resultsmotifs)
resultsmotifs$DATE <- as.numeric(as.character(resultsmotifs$DATE))
#dependent variable = normalized conditional entropy of authorities given motifs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(CEAuthoritiesMotifs ~ (1|MOTIFS), data = resultsmotifs)
testD <- lmer(CEAuthoritiesMotifs ~ (1|MOTIFS) + DATE, data = resultsmotifs)

summary(test0)
summary(testD)

anova(test0,testD)

#____________________________________________________________#
#REGRESSION ANALYSIS: GROUPING BY MOTIFS - performed on normalized values

resultsmotifs$MOTIFS <- rownames(resultsmotifs)
resultsmotifs$DATE <- as.numeric(as.character(resultsmotifs$DATE))
#dependent variable = normalized conditional entropy of authorities given motifs
#predictor = DATE; 13 time periods defined by the median year (e.g. 370 is a median year in a period 400-380 BCE)

require(lme4)
test0 <- lmer(NormCEAuthoritiesMotifs ~ (1|MOTIFS), data = resultsmotifs)
testD <- lmer(NormCEAuthoritiesMotifs ~ (1|MOTIFS) + DATE, data = resultsmotifs)

summary(test0)
summary(testD)

anova(test0,testD)

