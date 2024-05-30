# mr_phewas_fm_ffm

In the 'regenie_analyses' folder:
The .ipynb file contains code constructing the UK Biobank White British samples (phenotype files) used in REGENIE GWAS of fat mass and fat-free mass.

The .sh files (to be run sequentially) contain code for the REGENIE GWAS of fat mass and fat-free mass. This code was in part making use of resources and tutorials produced by DNA Nexus UK Biobank Research Analyses platform teams and available at the the pjgreer/ukb-rap-tools repository.

The .R files contain code for the two-sample phenome-wide Mendelian randomization analyses performed using the UK Biobank GWAS exposure data annd Finnish GWAS data on 821 outcomes: 1) produces the Manhattan plots; 2) constructs the genetic instruments; 3) selects the relevant outcomes and performs power calculations; 4) performs the MR (and sensitivity MR) analyses; 5) processes the MR results; 6) produces the forest plots of estimates from MR analyses.
