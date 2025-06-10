This folder contains files used for the analysis of the Prediction 3

- input file:	2018_01_10_CoinImagesAll.csv

- P3_analysis
	- analysis on data subsets
	- R code for analysis
	- R markup

- P3_full.csv -> conditional entropy of denominations and authorities given designs for subsets of data
	- split by denominations:	HIGHER denominations = larger than the base value
					LOWER denominations = smaller than or equal to the base value
	- split by number of motifs on a coin type:	MORE = more than the median number of motifs (=3)
							LESS = less than or equal to the median number of motifs (=3)

- P3test_byauth_higher.csv -> conditional entropy of denominations given designs measured by authorities for higher denominations
	- CEDenomination.Designs = conditional entropy of denominations given designs
	- Entropy.Denominations = entropy of denominations
	- NormCEDenomination.Designs = normalized conditional entropy of denominations given designs
	- Ncoins = number of coin types in the subset

- P3test_byauth_lower.csv -> conditional entropy of denominations given designs measured by authorities for lower denominations
	- CEDenomination.Designs = conditional entropy of denominations given designs
	- Entropy.Denominations = entropy of denominations
	- NormCEDenomination.Designs = normalized conditional entropy of denominations given designs
	- Ncoins = number of coin types in the subset

- P3test_bynofMots_higher.csv -> conditional entropy of denominations given designs measured for subsets of coin types with same number of motifs; for higher denominations
	- NUMofMotifs = number of motifs on coin types
	- CEDenomination.Designs = conditional entropy of denominations given designs
	- NormCEDenomination.Designs = normalized conditional entropy of denominations given designs
	- Ncoins = number of coin types in the subset

- P3test_bynofMots_lower.csv -> conditional entropy of denominations given designs measured for subsets of coin types with same number of motifs; for lower denominations
	- NUMofMotifs = number of motifs on coin types
	- CEDenomination.Designs = conditional entropy of denominations given designs
	- NormCEDenomination.Designs = normalized conditional entropy of denominations given designs
	- Ncoins = number of coin types in the subset