Folders:

raw/: Contains raw datasets (fips_counties.csv,
	Hospital_Inpatient_Discharges__SPARCS_De-Identified___2019.csv,
	Hospital_Inpatient_Discharges__SPARCS_De-Identified___2021.csv).

data/: Contains pre-processed datasets (diab_2019.Rda, diab_2021.Rda, diab_all.Rda,
	diab_all_with_predictions.Rda, fips_counties.xlsx, county_id_label.Rda).

doc/: Contains documentation regarding raw datasets.

output/: Contains all the plots and tables that are produced.

program/: Contains the script in R to be run in the following order:

- 00_data_manipulation.R (Optional): Script for generating pre-processed datasets
	(It can be skipped as the following scripts are optimized for pre-processed
 	datasets; loading raw data takes approximately 6 minutes).

- 01_summary_statistics.R: Generates summary statistics.

- 02_data_exploration.R: Produces maps and other charts for exploratory analysis.

- 03_direct_standardization.R: Performs direct standardization to compute standardized
	rates and visualizes differences across groups.

- 04_indirect_standardization.R: Implements indirect standardization methods, including
	funnel plots to compare observed and expected rates.

- 05_models.R: Fits logistic regression and generalized estimating equation (GEE)
	models. Also includes ROC curve analysis, AUC computation, and determination
	of optimal cut-off points using the Youden index.

