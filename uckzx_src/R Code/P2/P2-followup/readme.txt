This folder contains files used for the follow-up analysis of the Prediction 2

- input file:	2018_01_10_CoinImagesAll.csv

- P2_followup_newbins:

	- design-based analysis (OM&BP)
	- R code for the analysis made on 13 time periods (finegrained)
	- R html markdown

	- P2_followup_newbins_results.csv -> conditional entropy of designs given denominations calculated by time period
		- DATE = 13 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEMotifsDenominations = conditional entropy of designsgiven denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of designsgiven denominations (CEMotifsDenominations  / HMotifs)


	- P2_followup_newbins_results_bypolis -> conditional entropy of designs given denominations calculated for each authority by time period
		- DATE = 13 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEMotifsDenominations = conditional entropy of designsgiven denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of designs given denominations (CEMotifsDenominations  / HMotifs)

