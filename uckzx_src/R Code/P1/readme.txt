This folder contains files used for the analysis of the Prediction 1

- input file:	2018_01_10_CoinImagesAll.csv

- P1_analysis_newbins:

	- design-based analysis (OM&BP)
	- R code for the analysis made on 13 time periods (finegrained)
	- R markup

	- P1analysis_newbins_results.csv - conditional entropy of authorities given motifs calculated by time period
		- DATE = 13 time periods
		- HAuthority = entropy of authorities
		- HMotifs = entropy of motifs
		- CEAuthoritiesMotifs = conditional entropy of authorities given motifs
		- NormCEAuthoritiesMotifs = normalised conditional entropy of authorities given motifs (CEAuthoritiesMotifs  / HAuthority)
		- CEMotifsAuthorities = conditional entropy of motifs given authorities
		- NormCEMotifsAuthorities =  normalised conditional entropy of motifs given authorities (CEMotifsAuthorities  / HMotifs)
		- MI = mutual information of authorities and motifs
		- NAuthorities = number of unique authorities inside a time period
		- NCoins = number of coin types (observations) inside a time period

	
- P1_motif-by-motif_newbins:

	- motifs-based analysis (BP)
	- R code for the analysis made on 13 time periods (finegrained)
	- R markup

	- P1analysis_newbins_results_bymotif.csv - conditional entropy of authorities given motifs calculated for each motif by time period
		- DATE = 13 time periods
		- HAuthority = entropy of authorities
		- HMotifs = entropy of motifs
		- CEAuthoritiesMotifs = conditional entropy of authorities given motifs
		- NormCEAuthoritiesMotifs = normalised conditional entropy of authorities given motifs (CEAuthoritiesMotifs  / HAuthority)
		- CEMotifsAuthorities = conditional entropy of motifs given authorities
		- NormCEMotifsAuthorities =  normalised conditional entropy of motifs given authorities (CEMotifsAuthorities  / HMotifs)
		- MI = mutual information of authorities and motifs
	

								*******************************


- P1_analysis_oldbins: 

	- design-based analysis (OM&BP)
	- R code for the analysis made on 3 time periods (preregistered)
	- R markup

	- P1analysis_oldbins_results.csv - conditional entropy of authorities given motifs calculated by time period
		- DATE = 3 time periods
		- HAuthority = entropy of authorities
		- HMotifs = entropy of motifs
		- CEAuthoritiesMotifs = conditional entropy of authorities given motifs
		- NormCEAuthoritiesMotifs = normalised conditional entropy of authorities given motifs (CEAuthoritiesMotifs  / HAuthority)
		- CEMotifsAuthorities = conditional entropy of motifs given authorities
		- NormCEMotifsAuthorities =  normalised conditional entropy of motifs given authorities (CEMotifsAuthorities  / HMotifs)
		- MI = mutual information of authorities and motifs
		- NAuthorities = number of unique authorities inside a time period
		- NCoins = number of coin types (observations) inside a time period


- P1_motif-by-motif_oldbins:

	- motifs-based analysis (BP)
	- R code for the analysis made on 3 time periods (preregistered)
	- R markup

	- P1analysis_oldbins_results_bymotif.csv - conditional entropy of authorities given motifs calculated for each motif by time period
		- DATE = 13 time periods
		- HAuthority = entropy of authorities
		- HMotifs = entropy of motifs
		- CEAuthoritiesMotifs = conditional entropy of authorities given motifs
		- NormCEAuthoritiesMotifs = normalised conditional entropy of authorities given motifs (CEAuthoritiesMotifs  / HAuthority)
		- CEMotifsAuthorities = conditional entropy of motifs given authorities
		- NormCEMotifsAuthorities =  normalised conditional entropy of motifs given authorities (CEMotifsAuthorities  / HMotifs)
		- MI = mutual information of authorities and motifs