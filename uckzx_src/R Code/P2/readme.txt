This folder contains files used for the analysis of the Prediction 2

- input file:	2018_01_10_CoinImagesAll.csv

- P2_analysis_newbins:

	- designs-based analysis (OM&BP)
	- R code for the analysis made on 13 time periods (finegrained)
	- R html markdown

	- P2analysis_newbins_results.csv - conditional entropy of denominations given motifs calculated by time period
		- DATE = 13 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs
		- NDenominations = number of unique denominations inside a time period
		- NCoins = number of coin types (observations) inside a time period

	- P2analysis_newbins_results_bypolis - conditional entropy of denominations given motifs calculated for each authority by time period
		- DATE = 13 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs


- P2_motif-by-motif_newbins:

	- motif-based analysis (BP)
	- R code for the analysis made on 13 time periods (finegrained)
	- R html markdown

	- P2analysis_newbins_results_bymotif - conditional entropy of denominations given motifs calculated for each motif by time period
		- DATE = 13 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs

	

								*******************************


- P2_analysis_oldbins: 

	- designs-based analysis (OM&BP)
	- R code for the analysis made on 3 time periods (preregistered)
	- R html markdown

	- P2analysis_oldbins_results.csv - conditional entropy of denominations given motifs calculated by time period
		- DATE = 3 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs
		- NDenominations = number of unique denominations inside a time period
		- NCoins = number of coin types (observations) inside a time period

	- P2analysis_oldbins_results_bypolis - conditional entropy of denominations given motifs calculated for each authority by time period
		- DATE = 3 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs


- P2_motif-by-motif_oldbins:

	- motif-based analysis (BP)
	- R code for the analysis made on 3 time periods (preregistered)
	- R html markdown

	- P2analysis_oldbins_results_bymotif - conditional entropy of denominations given motifs calculated for each motif by time period
		- DATE = 3 time periods
		- HDenomination = entropy of denominations
		- HMotifs = entropy of motifs
		- CEADenominationsMotifs = conditional entropy of denominations given motifs
		- NormCEDenominationsMotifs = normalised conditional entropy of denominations given motifs (CEDenominationsMotifs  / HDenomination)
		- CEMotifsDenominations = conditional entropy of motifs given denominations
		- NormCEMotifsDenominations =  normalised conditional entropy of motifs given denominations (CEMotifsDenominations  / HMotifs)
		- MI = mutual information of denominations and motifs
