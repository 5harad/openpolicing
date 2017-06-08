This repository contains the code and accessory files needed to process the state data and reproduce the analysis in the Stanford Open Policing Project. It is organized as follows: 
  
  * `/data_and_models` stores the state data and fitted models. It is initially empty.
  * `/resources` contains small accessory files needed for the analysis: for example, mappings from the raw data values to standardized data values. 
  * `/results` contains the tables, figures, and data underlying the figures in the paper. 
  * `/src` contains the code.
  * `/tutorial` contains R tutorials for analyzing the data.

## Instructions for working with the data and reproducing results in the paper

1. Download the state data from the Open Policing website and store it in `data_and_models/data/`. Each state should be stored in its own folder using a two-letter state code in the following format: "TWO_LETTER_CODE/TWO_LETTER_CODE-clean.csv.gz". For example, Rhode Island would be stored in "RI/RI-clean.csv.gz".
2. Download the external datasets necessary for the analysis -- the Census data and the PPCS data -- from the Open Policing website and store them in `data_and_models/external_datasets/`. 
3. The video tutorial posted on the Open Policing website provides an introduction to performing analyses on the data. 
4. The script used to reproduce all results in the paper is `src/recreate_results_in_paper.R`. All analyses should be run using this script, not by running other scripts directly, since it sources the necessary dependencies. To run this script:

    * Set code_path and data_and_models_path to suit your system. (If you have large computing resources, you can also set n_cores to be greater than 1. Rerunning all analyses will require extensive computing resources.) 
    * install the libraries necessary to run the analysis by setting install_libraries = TRUE. 
    * Set the TRUE/FALSE flags in the "Which analyses to run" section of the code to run the analyses you want to run. For example, if you wanted to rerun the threshold test models, you would set rerun_threshold_test = TRUE. 
    * Note that some analyses must precede others. Specifically, to run most of the analyses you must first compute aggregate statistics for each state which are much faster to work with; you can do this by setting reaggregate = TRUE. Some figures and tables in the paper rely on previous analyses. For example, before creating the regression tables, you must run the regressions by setting rerun_regressions = TRUE. You should be able to reproduce the results in the paper by 1. running reaggregate; 2. running rerun_regressions and rerun_threshold_test; 3. running remake_plots, remake_tables, print_aggregate_rates, and reproduce_miscellaneous_claims. 
    * The function of each analysis script is described in `src/analysis/README.md`. 



