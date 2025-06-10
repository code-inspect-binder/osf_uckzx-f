#HEADS & TAILS: Informational value of Greek coinage

#Prediction 1: Motifs become less informative about a coin's provenance.

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
HAuthority <- c() #Entropy of authority
HMotifs <- c() #Entropy of motifs
CEAuthoritiesMotifs <- c() #Conditional entropy of authority given designs
NormCEAuthoritiesMotifs <- c() #Normalized conditional entropy of authority given designs
MI <- c() #Mutual information between authority and motifs
CEMotifsAuthorities <- c() #Conditional entropy of motifs given authority
NormCEMotifsAuthorities <- c() #Normalized conditional entropy of motifs given authority
NAuthorities <- c() #Number of unique authorities inside a period
NCoins <- c() #Number of coin types inside a period (number of observations)

#Creating subsets of coins by time period and obtaining measures for each of the time periods
for (i in dates) { 
  dsub <- (subset(df, df$DATE == i))
  motifs <- cbind(dsub[318:681])
  autho <- cbind(dsub[3:254])  
  DATE[i] <- i
  HAuthority[i] <- entropy(autho)
  HMotifs[i] <- entropy(motifs)
  CEAuthoritiesMotifs[i] <- condentropy(autho, motifs)
  NormCEAuthoritiesMotifs[i] <- condentropy(autho, motifs) / entropy(autho)
  CEMotifsAuthorities[i] <- condentropy(motifs, autho)
  NormCEMotifsAuthorities[i] <- condentropy(motifs, autho) / entropy(motifs)
  MI[i] <- mutinformation(autho, motifs)
  NAuthorities[i] <- length(unique(dsub$AUTHORITY))
  NCoins[i] <- length(unique(dsub$ID))
}

#Combining vectors into a results data frame
results = as.data.frame(cbind(DATE, HAuthority, HMotifs, CEAuthoritiesMotifs, NormCEAuthoritiesMotifs, 
                              CEMotifsAuthorities, NormCEMotifsAuthorities, MI, NAuthorities, NCoins))
results <- na.omit(as.data.frame(results))

write.csv2(results, "P1analysis_newbins_results.csv")


#____________________________________________________________#
#STATISTICAL TEST: non-parametric Spearman's correlation

#non-Normalized conditional entropy
cor.test(results$DATE, results$CEAuthoritiesMotifs, method = "spearman") 

rdates <- rev(dates) #dates = years BCE
plot(results$DATE, results$CEAuthoritiesMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(A|d)", main = "P1: Conditional entropy of authorities given designs")
axis(1, at = rdates, labels = rdates)


#Normalized conditional entropy
cor.test(results$DATE, results$NormCEAuthoritiesMotifs, method = "spearman") 

plot(results$DATE, results$NormCEAuthoritiesMotifs, xlim = c(600,330), xaxt='n', xlab = "YEAR BCE", 
     ylab = "H(A|d) / H(A)", main = "P1: Normalized conditional entropy of authorities given designs")
axis(1, at = rdates, labels = rdates)
