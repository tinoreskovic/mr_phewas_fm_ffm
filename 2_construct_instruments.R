#install.packages("dplyr")
library(dplyr)
#install.packages("dplyr")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("MendelianRandomization")
library(MendelianRandomization)
#install.packages("TwoSampleMR")
library(TwoSampleMR)
##install.packages("dplyr")
library(ggplot2)
#install.packages("dplyr")
library(stringr)
#install.packages("dplyr")
library(wesanderson)
#install.packages("R.utils")
library(R.utils)
#install.packages("ckbplotr")
library(ckbplotr)
#remotes::install_github("mrcieu/ieugwasr")
library(ieugwasr)
#install.packages("metafor")
library(metafor)
#install.packages("flextable")
library(flextable)
#install.packages("qqman")
library(qqman)
#install.packages("grid")
library(grid)
#install.packages("gridGraphics")
library(gridGraphics)
#install.packages("ggtext")
library(ggtext)
#install.packages("curl")
library(curl)



fat_mass_regenie <- read.delim("path_to_merged_regenie_output/assoc.fat_mass_37.regenie.merged.txt")


fat_mass_regenie <- 
  fat_mass_regenie %>% mutate(P = (10^(-LOG10P)))


fat_mass_regenie <- 
  fat_mass_regenie %>%
  filter(TEST == "ADD") %>%
  tidyr::drop_na(LOG10P)


fat_mass_regenie$pval.exposure <- fat_mass_regenie$P
fat_mass_regenie$id.exposure <- "Whole body fat mass"
fat_mass_regenie$chr_name <- fat_mass_regenie$CHROM
fat_mass_regenie$chrom_start <- fat_mass_regenie$GENPOS
fat_mass_regenie$SNP <- fat_mass_regenie$ID

fat_free_mass_regenie <- read.delim("path_to_merged_regenie_output/assoc.fat_free_mass_37.regenie.merged.txt")


fat_free_mass_regenie <- 
  fat_free_mass_regenie %>% mutate(P = (10^(-LOG10P)))


fat_free_mass_regenie <- 
  fat_free_mass_regenie %>%
  filter(TEST == "ADD") %>%
  tidyr::drop_na(LOG10P)

fat_mass_regenie$pval.exposure <- fat_mass_regenie$P
fat_mass_regenie$id.exposure <- "Whole body fat mass"
fat_mass_regenie$chr_name <- fat_mass_regenie$CHROM
fat_mass_regenie$chrom_start <- fat_mass_regenie$GENPOS
fat_mass_regenie$SNP <- fat_mass_regenie$ID

fm_instruments_ukb <- clump_data(
  fat_mass_regenie,
  clump_kb = 10000,
  clump_r2 = 0.01,
  clump_p1 = 5e-08,
  clump_p2 = 5e-08,
  pop = "EUR"
)


fm_instruments_ukb$exposure <- fm_instruments_ukb$id.exposure
fm_instruments_ukb$beta.exposure <- fm_instruments_ukb$BETA
fm_instruments_ukb$se.exposure <- fm_instruments_ukb$SE
fm_instruments_ukb$effect_allele.exposure <- fm_instruments_ukb$ALLELE1 
fm_instruments_ukb$other_allele.exposure <- fm_instruments_ukb$ALLELE0
fm_instruments_ukb$eaf.exposure <- fm_instruments_ukb$A1FREQ
fm_instruments_ukb$samplesize.exposure <- 400154



fat_free_mass_regenie$pval.exposure <- fat_free_mass_regenie$P
fat_free_mass_regenie$id.exposure <- "Whole body fat-free mass"
fat_free_mass_regenie$chr_name <- fat_free_mass_regenie$CHROM
fat_free_mass_regenie$chrom_start <- fat_free_mass_regenie$GENPOS
fat_free_mass_regenie$SNP <- fat_free_mass_regenie$ID


ffm_instruments_ukb <- clump_data(
  fat_free_mass_regenie,
  clump_kb = 10000,
  clump_r2 = 0.01,
  clump_p1 = 5e-08,
  clump_p2 = 5e-08,
  pop = "EUR"
)


ffm_instruments_ukb$exposure <- ffm_instruments_ukb$id.exposure
ffm_instruments_ukb$beta.exposure <- ffm_instruments_ukb$BETA
ffm_instruments_ukb$se.exposure <- ffm_instruments_ukb$SE
ffm_instruments_ukb$effect_allele.exposure <- ffm_instruments_ukb$ALLELE1 
ffm_instruments_ukb$other_allele.exposure <- ffm_instruments_ukb$ALLELE0
ffm_instruments_ukb$eaf.exposure <- ffm_instruments_ukb$A1FREQ
ffm_instruments_ukb$samplesize.exposure <- 400154


mv_extract_exposures_regenie <- function(gwas1=fat_mass_regenie, gwas2=fat_free_mass_regenie, clump_r2=0.01, clump_kb=10000, harmonise_strictness=2){
  require(reshape2)
  
  
  # Get best instruments for each exposure
  exposure_dat <- rbind(fm_instruments_ukb, ffm_instruments_ukb)
  temp <- exposure_dat
  temp$id.exposure <- 1
  temp <- clump_data(temp, clump_r2=clump_r2, clump_kb=clump_kb)
  exposure_dat <- subset(exposure_dat, SNP %in% temp$SNP)
  
  
  # Get effects of each instrument from each exposure
  
  d1_a <- format_data(dat=gwas1, type="exposure",
                      snps=exposure_dat$SNP, phenotype_col = "id.exposure",
                      beta_col="BETA", se_col="SE", eaf_col="A1FREQ",
                      effect_allele_col = "ALLELE1",
                      other_allele_col = "ALLELE0",
                      pval_col = "P")
  
  d1_b <- format_data(dat=gwas2, type="exposure",
                      snps=exposure_dat$SNP, phenotype_col = "id.exposure",
                      beta_col="BETA", se_col="SE", eaf_col="A1FREQ",
                      effect_allele_col = "ALLELE1",
                      other_allele_col = "ALLELE0",
                      pval_col = "P")
  
  dh <- rbind(d1_a, d1_b)
  
  dh <- subset(dh, select = -c(mr_keep.exposure, pval_origin.exposure))
  
  return(dh)
}


exposure_dat <- mv_extract_exposures_regenie(gwas1=fat_mass_regenie, gwas2=fat_free_mass_regenie, clump_r2=0.01, clump_kb=10000, harmonise_strictness=2)

#Incorporating proxies for SNPs not present in FinnGen

outcome_snps <- data.table::fread("https://storage.googleapis.com/finngen-public-data-r8/summary_stats/finngen_R8_I9_MI_STRICT.gz")


outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = fm_instruments_ukb$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

length(unique(fm_instruments_ukb$SNP))-length(unique(outcome$SNP)) 

#52 OUT of 637  MISSING IN FINNGEN

snps_in_finngen <- unique(outcome$SNP)

missing_snps <- subset(fm_instruments_ukb, !(fm_instruments_ukb$SNP %in% snps_in_finngen))

nrow(missing_snps) #52 SNPS MISSING


LDproxy_batch(missing_snps$SNP, 
              pop = "EUR", 
              r2d = "r2", 
              token = "5d3ad7d2482b", 
              append = TRUE,
              genome_build = "grch37"
             )

proxies_fm <- read.delim("path_to_LDproxy_batch_output/combined_query_snp_list_grch37.txt", row.names=NULL)

proxies_fm <- proxies_fm[!duplicated(proxies_fm), ]
length(unique(proxies_fm$query_snp))

proxies_fm <- subset(proxies_fm, proxies_fm$R2 >= 0.8)
length(unique(proxies_fm$query_snp))


proxies_in_finngen <- format_data(
  outcome_snps,
  type = "outcome",
  snps = proxies_fm$RS_Number,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

keepsnps <- unique(proxies_in_finngen$SNP)

length(unique(proxies_fm$query_snp)) 
proxies_fm <- subset(proxies_fm, proxies_fm$RS_Number %in% keepsnps)
length(unique(proxies_fm$query_snp)) 

proxies_fm$occurrence <- ave(proxies_fm$query_snp, proxies_fm$query_snp, FUN = seq_along)

nrow(subset(proxies_fm, proxies_fm$query_snp==proxies_fm$RS_Number))

proxies_fm <- subset(proxies_fm, proxies_fm$occurrence==1)

proxies_fm$Alleles <- gsub("[()]", "", proxies_fm$Alleles)

# Extract substrings from the "Alleles" column and create new columns
alleles_split <- strsplit(as.character(proxies_fm$Alleles), "/")

# Create the "ALLELE0_p" and "ALLELE1_p" columns
proxies_fm$ALLELE0_p <- sapply(alleles_split, "[[", 1)
proxies_fm$ALLELE1_p <- sapply(alleles_split, "[[", 2)
proxies_fm$other_allele.exposure_p <- proxies_fm$ALLELE0_p
proxies_fm$effect_allele.exposure_p <- proxies_fm$ALLELE1_p

proxies_fm$A1FREQ_p <- proxies_fm$MAF
proxies_fm$eaf.exposure_p <- proxies_fm$A1FREQ_p

proxies_fm$Coord <- gsub("chr", "", proxies_fm$Coord)

# Split the "Coord" column by ":"
coord_split <- strsplit(as.character(proxies_fm$Coord), ":")

# Create the "chr_name_p" and "chrom_start_p" columns
proxies_fm$chr_name_p <- sapply(coord_split, "[[", 1)
proxies_fm$chrom_start_p <- sapply(coord_split, "[[", 2)

proxies_fm$SNP_p <- proxies_fm$RS_Number
proxies_fm$ID_p <- proxies_fm$RS_Number
proxies_fm <- proxies_fm[, c(2, 15:24)]

fm_instruments_ukb <- merge(fm_instruments_ukb, proxies_fm, by.x="SNP", by.y = "query_snp", all.x = TRUE)

fm_instruments_ukb$SNP <- ifelse(is.na(fm_instruments_ukb$SNP_p),
                                 fm_instruments_ukb$SNP,
                                 fm_instruments_ukb$SNP_p)
fm_instruments_ukb$ID <- ifelse(is.na(fm_instruments_ukb$ID_p),
                                fm_instruments_ukb$ID,
                                fm_instruments_ukb$ID_p)
fm_instruments_ukb$chr_name <- ifelse(is.na(fm_instruments_ukb$chr_name_p),
                                      fm_instruments_ukb$chr_name,
                                      fm_instruments_ukb$chr_name_p)
fm_instruments_ukb$chrom_start <- ifelse(is.na(fm_instruments_ukb$chrom_start_p),
                                         fm_instruments_ukb$chrom_start,
                                         fm_instruments_ukb$chrom_start_p)

fm_instruments_ukb$A1FREQ <- ifelse(is.na(fm_instruments_ukb$A1FREQ_p),
                                    fm_instruments_ukb$A1FREQ,
                                    fm_instruments_ukb$A1FREQ_p)

fm_instruments_ukb$ALLELE0 <- ifelse(is.na(fm_instruments_ukb$ALLELE0_p),
                                     fm_instruments_ukb$ALLELE0,
                                     fm_instruments_ukb$ALLELE0_p)
fm_instruments_ukb$ALLELE1 <- ifelse(is.na(fm_instruments_ukb$ALLELE1_p),
                                     fm_instruments_ukb$ALLELE1,
                                     fm_instruments_ukb$ALLELE1_p)

fm_instruments_ukb$eaf.exposure <- ifelse(is.na(fm_instruments_ukb$eaf.exposure_p),
                                          fm_instruments_ukb$eaf.exposure,
                                          fm_instruments_ukb$eaf.exposure_p)

fm_instruments_ukb$effect_allele.exposure <- ifelse(is.na(fm_instruments_ukb$effect_allele.exposure_p),
                                                    fm_instruments_ukb$effect_allele.exposure,
                                                    fm_instruments_ukb$effect_allele.exposure_p)


fm_instruments_ukb$other_allele.exposure <- ifelse(is.na(fm_instruments_ukb$other_allele.exposure_p),
                                                   fm_instruments_ukb$other_allele.exposure,
                                                   fm_instruments_ukb$other_allele.exposure_p)


outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = fm_instruments_ukb$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

nrow(subset(fm_instruments_ukb,!is.na(fm_instruments_ukb$other_allele.exposure_p)))
#46 out of 52 found proxies for generally, 631 in total

length(unique(outcome$SNP)) #631 out of 637 in finngen (all proxies found in finngen!)

fm_instruments_ukb <- fm_instruments_ukb[, !grepl("_p$", colnames(fm_instruments_ukb))]

dat <- harmonise_data(fm_instruments_ukb, outcome)

nrow(dat)-nrow(subset(dat, dat$mr_keep==FALSE)) #602 finally, further 29 removed due to being palindromic with intermediate allele frequences, incompatible alleles 

keepsnps <- unique(subset(dat, dat$mr_keep==TRUE)$SNP)

fm_instruments_ukb <- subset(fm_instruments_ukb, fm_instruments_ukb$SNP %in% keepsnps)

#Harmonising Whole body fat mass (Whole body fat mass) and outcome (nZHBkV)
#Removing the following SNPs for incompatible alleles:
#  rs12583517, rs146643467, rs34898535
#Removing the following SNPs for being palindromic with intermediate allele frequencies:
#  rs10059453, rs10792, rs10887578, rs1147345, rs1188178, rs12927987, rs13047416, rs13209968, rs13264909, rs1454687, rs146728296, rs2238435, rs2253310, rs254025, rs2605593, rs3943933, rs4419475, rs4780885, rs4944500, rs537508, rs6583310, rs6597653, rs6685593, rs6888042, rs6899218, rs7491529



outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = ffm_instruments_ukb$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

length(unique(ffm_instruments_ukb$SNP))-length(unique(outcome$SNP)) 

#77 OUT of 1118  MISSING IN FINNGEN

snps_in_finngen <- unique(outcome$SNP)

missing_snps <- subset(ffm_instruments_ukb, !(ffm_instruments_ukb$SNP %in% snps_in_finngen))

nrow(missing_snps) #77 SNPS MISSING


LDproxy_batch(missing_snps$SNP, 
              pop = "EUR", 
              r2d = "r2", 
              token = "5d3ad7d2482b", 
              append = TRUE,
              genome_build = "grch37"
             )

proxies_fm <- read.delim("path_to_LDproxy_batch_output/combined_query_snp_list_grch37.txt", row.names=NULL)

proxies_fm <- proxies_fm[!duplicated(proxies_fm), ]
length(unique(proxies_fm$query_snp))

proxies_fm <- subset(proxies_fm, proxies_fm$R2 >= 0.8)
length(unique(proxies_fm$query_snp))


proxies_in_finngen <- format_data(
  outcome_snps,
  type = "outcome",
  snps = proxies_fm$RS_Number,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

keepsnps <- unique(proxies_in_finngen$SNP)

length(unique(proxies_fm$query_snp)) 
proxies_fm <- subset(proxies_fm, proxies_fm$RS_Number %in% keepsnps)
length(unique(proxies_fm$query_snp)) 

proxies_fm$occurrence <- ave(proxies_fm$query_snp, proxies_fm$query_snp, FUN = seq_along)

nrow(subset(proxies_fm, proxies_fm$query_snp==proxies_fm$RS_Number))

proxies_fm <- subset(proxies_fm, proxies_fm$occurrence==1)

proxies_fm$Alleles <- gsub("[()]", "", proxies_fm$Alleles)

# Extract substrings from the "Alleles" column and create new columns
alleles_split <- strsplit(as.character(proxies_fm$Alleles), "/")

# Create the "ALLELE0_p" and "ALLELE1_p" columns
proxies_fm$ALLELE0_p <- sapply(alleles_split, "[[", 1)
proxies_fm$ALLELE1_p <- sapply(alleles_split, "[[", 2)
proxies_fm$other_allele.exposure_p <- proxies_fm$ALLELE0_p
proxies_fm$effect_allele.exposure_p <- proxies_fm$ALLELE1_p

proxies_fm$A1FREQ_p <- proxies_fm$MAF
proxies_fm$eaf.exposure_p <- proxies_fm$A1FREQ_p

proxies_fm$Coord <- gsub("chr", "", proxies_fm$Coord)

# Split the "Coord" column by ":"
coord_split <- strsplit(as.character(proxies_fm$Coord), ":")

# Create the "chr_name_p" and "chrom_start_p" columns
proxies_fm$chr_name_p <- sapply(coord_split, "[[", 1)
proxies_fm$chrom_start_p <- sapply(coord_split, "[[", 2)

proxies_fm$SNP_p <- proxies_fm$RS_Number
proxies_fm$ID_p <- proxies_fm$RS_Number
proxies_fm <- proxies_fm[, c(2, 15:24)]

ffm_instruments_ukb <- merge(ffm_instruments_ukb, proxies_fm, by.x="SNP", by.y = "query_snp", all.x = TRUE)

ffm_instruments_ukb$SNP <- ifelse(is.na(ffm_instruments_ukb$SNP_p),
                                  ffm_instruments_ukb$SNP,
                                  ffm_instruments_ukb$SNP_p)
ffm_instruments_ukb$ID <- ifelse(is.na(ffm_instruments_ukb$ID_p),
                                 ffm_instruments_ukb$ID,
                                 ffm_instruments_ukb$ID_p)
ffm_instruments_ukb$chr_name <- ifelse(is.na(ffm_instruments_ukb$chr_name_p),
                                       ffm_instruments_ukb$chr_name,
                                       ffm_instruments_ukb$chr_name_p)
ffm_instruments_ukb$chrom_start <- ifelse(is.na(ffm_instruments_ukb$chrom_start_p),
                                          ffm_instruments_ukb$chrom_start,
                                          ffm_instruments_ukb$chrom_start_p)

ffm_instruments_ukb$A1FREQ <- ifelse(is.na(ffm_instruments_ukb$A1FREQ_p),
                                     ffm_instruments_ukb$A1FREQ,
                                     ffm_instruments_ukb$A1FREQ_p)

ffm_instruments_ukb$ALLELE0 <- ifelse(is.na(ffm_instruments_ukb$ALLELE0_p),
                                      ffm_instruments_ukb$ALLELE0,
                                      ffm_instruments_ukb$ALLELE0_p)
ffm_instruments_ukb$ALLELE1 <- ifelse(is.na(ffm_instruments_ukb$ALLELE1_p),
                                      ffm_instruments_ukb$ALLELE1,
                                      ffm_instruments_ukb$ALLELE1_p)

ffm_instruments_ukb$eaf.exposure <- ifelse(is.na(ffm_instruments_ukb$eaf.exposure_p),
                                           ffm_instruments_ukb$eaf.exposure,
                                           ffm_instruments_ukb$eaf.exposure_p)

ffm_instruments_ukb$effect_allele.exposure <- ifelse(is.na(ffm_instruments_ukb$effect_allele.exposure_p),
                                                     ffm_instruments_ukb$effect_allele.exposure,
                                                     ffm_instruments_ukb$effect_allele.exposure_p)


ffm_instruments_ukb$other_allele.exposure <- ifelse(is.na(ffm_instruments_ukb$other_allele.exposure_p),
                                                    ffm_instruments_ukb$other_allele.exposure,
                                                    ffm_instruments_ukb$other_allele.exposure_p)


outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = ffm_instruments_ukb$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

nrow(subset(ffm_instruments_ukb,!is.na(ffm_instruments_ukb$other_allele.exposure_p)))
#63 out of 77 found proxies for generally, 1104 in total

length(unique(outcome$SNP)) #1104 out of 1118 in finngen (all proxies found in finngen!)

ffm_instruments_ukb <- ffm_instruments_ukb[, !grepl("_p$", colnames(ffm_instruments_ukb))]

dat <- harmonise_data(ffm_instruments_ukb, outcome)

nrow(dat)-nrow(subset(dat, dat$mr_keep==FALSE)) #1059 finally, further 45 removed due to being palindromic with intermediate allele frequences, incompatible alleles 

keepsnps <- unique(subset(dat, dat$mr_keep==TRUE)$SNP)

ffm_instruments_ukb <- subset(ffm_instruments_ukb, ffm_instruments_ukb$SNP %in% keepsnps)

#Harmonising Whole body fat-free mass (Whole body fat-free mass) and outcome (q6jVn9)
#Removing the following SNPs for incompatible alleles:
#  rs12581771, rs35228793, rs3740591
#Removing the following SNPs for being palindromic with intermediate allele frequencies:
#  rs1024853, rs10838202, rs1093019, rs11234242, rs11881338, rs12507026, rs12572775, rs12631813, rs12897618, rs13047416, rs13264909, rs1454687, rs1561298, rs1652946, rs2238435, rs2284746, rs2305918, rs3749387, rs3771382, rs3790731, rs411833, rs4360494, rs4677150, rs4769763, rs4780885, rs4837613, rs4887925, rs597539, rs607472, rs6441042, rs6461943, rs67736411, rs7020201, rs7652177, rs7709645, rs7916441, rs7942152, rs874356, rs912690, rs9379130, rs9475170, rs9678029



outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = exposure_dat$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

length(unique(exposure_dat$SNP))-length(unique(outcome$SNP)) 

#94 OUT of 1344  MISSING IN FINNGEN

snps_in_finngen <- unique(outcome$SNP)

missing_snps <- subset(exposure_dat, !(exposure_dat$SNP %in% snps_in_finngen))

nrow(missing_snps)

LDproxy_batch(missing_snps$SNP, 
              pop = "EUR", 
              r2d = "r2", 
              token = "5d3ad7d2482b", 
              append = TRUE,
              genome_build = "grch37"
             )

proxies_fm <- read.delim("path_to_LDproxy_batch_output/combined_query_snp_list_grch37.txt", row.names=NULL)

proxies_fm <- proxies_fm[!duplicated(proxies_fm), ]
length(unique(proxies_fm$query_snp))

proxies_fm <- subset(proxies_fm, proxies_fm$R2 >= 0.8)
length(unique(proxies_fm$query_snp))


proxies_in_finngen <- format_data(
  outcome_snps,
  type = "outcome",
  snps = proxies_fm$RS_Number,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

keepsnps <- unique(proxies_in_finngen$SNP)

length(unique(proxies_fm$query_snp)) 
proxies_fm <- subset(proxies_fm, proxies_fm$RS_Number %in% keepsnps)
length(unique(proxies_fm$query_snp)) 

proxies_fm$occurrence <- ave(proxies_fm$query_snp, proxies_fm$query_snp, FUN = seq_along)

nrow(subset(proxies_fm, proxies_fm$query_snp==proxies_fm$RS_Number))

proxies_fm <- subset(proxies_fm, proxies_fm$occurrence==1)

proxies_fm$Alleles <- gsub("[()]", "", proxies_fm$Alleles)

# Extract substrings from the "Alleles" column and create new columns
alleles_split <- strsplit(as.character(proxies_fm$Alleles), "/")

# Create the "ALLELE0_p" and "ALLELE1_p" columns
proxies_fm$ALLELE0_p <- sapply(alleles_split, "[[", 1)
proxies_fm$ALLELE1_p <- sapply(alleles_split, "[[", 2)
proxies_fm$other_allele.exposure_p <- proxies_fm$ALLELE0_p
proxies_fm$effect_allele.exposure_p <- proxies_fm$ALLELE1_p

proxies_fm$A1FREQ_p <- proxies_fm$MAF
proxies_fm$eaf.exposure_p <- proxies_fm$A1FREQ_p

proxies_fm$Coord <- gsub("chr", "", proxies_fm$Coord)

# Split the "Coord" column by ":"
coord_split <- strsplit(as.character(proxies_fm$Coord), ":")

# Create the "chr_name_p" and "chrom_start_p" columns
proxies_fm$chr_name_p <- sapply(coord_split, "[[", 1)
proxies_fm$chrom_start_p <- sapply(coord_split, "[[", 2)

proxies_fm$SNP_p <- proxies_fm$RS_Number
proxies_fm$ID_p <- proxies_fm$RS_Number
proxies_fm <- proxies_fm[, c(2, 15:24)]

exposure_dat <- merge(exposure_dat, proxies_fm, by.x="SNP", by.y = "query_snp", all.x = TRUE)

exposure_dat$SNP <- ifelse(is.na(exposure_dat$SNP_p),
                           exposure_dat$SNP,
                           exposure_dat$SNP_p)
exposure_dat$ID <- ifelse(is.na(exposure_dat$ID_p),
                          exposure_dat$ID,
                          exposure_dat$ID_p)
exposure_dat$chr_name <- ifelse(is.na(exposure_dat$chr_name_p),
                                exposure_dat$chr_name,
                                exposure_dat$chr_name_p)
exposure_dat$chrom_start <- ifelse(is.na(exposure_dat$chrom_start_p),
                                   exposure_dat$chrom_start,
                                   exposure_dat$chrom_start_p)

exposure_dat$A1FREQ <- ifelse(is.na(exposure_dat$A1FREQ_p),
                              exposure_dat$A1FREQ,
                              exposure_dat$A1FREQ_p)

exposure_dat$ALLELE0 <- ifelse(is.na(exposure_dat$ALLELE0_p),
                               exposure_dat$ALLELE0,
                               exposure_dat$ALLELE0_p)
exposure_dat$ALLELE1 <- ifelse(is.na(exposure_dat$ALLELE1_p),
                               exposure_dat$ALLELE1,
                               exposure_dat$ALLELE1_p)

exposure_dat$eaf.exposure <- ifelse(is.na(exposure_dat$eaf.exposure_p),
                                    exposure_dat$eaf.exposure,
                                    exposure_dat$eaf.exposure_p)

exposure_dat$effect_allele.exposure <- ifelse(is.na(exposure_dat$effect_allele.exposure_p),
                                              exposure_dat$effect_allele.exposure,
                                              exposure_dat$effect_allele.exposure_p)


exposure_dat$other_allele.exposure <- ifelse(is.na(exposure_dat$other_allele.exposure_p),
                                             exposure_dat$other_allele.exposure,
                                             exposure_dat$other_allele.exposure_p)


outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = exposure_dat$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

nrow(subset(exposure_dat,!is.na(exposure_dat$other_allele.exposure_p)))/2
#78 out of 94 found proxies for generally, 1328 in total

length(unique(outcome$SNP)) #1328 out of 1344 in finngen (all proxies found in finngen!)

exposure_dat <- exposure_dat[, !grepl("_p$", colnames(exposure_dat))]

dat <- harmonise_data(exposure_dat, outcome)

(nrow(dat)-nrow(subset(dat, dat$mr_keep==FALSE)))/2 #1269 finally, further 59 removed due to being palindromic with intermediate allele frequences, incompatible alleles 

keepsnps <- unique(subset(dat, dat$mr_keep==TRUE)$SNP)

exposure_dat <- subset(exposure_dat, exposure_dat$SNP %in% keepsnps)

#Harmonising Whole body fat mass (6DZ2NY) and outcome (EPi0B6)
#Removing the following SNPs for incompatible alleles:
#  rs12581771, rs146643467, rs34898535, rs35228793, rs3740591
#Removing the following SNPs for being palindromic with intermediate allele frequencies:
#  rs1024853, rs10792, rs10887578, rs1093019, rs11234242, rs1147345, rs1188178, rs12572775, rs12631813, rs12897618, rs12927987, rs13047416, rs13264909, rs1454687, rs146728296, rs1561298, rs1652946, rs2238435, rs2284746, rs2305918, rs254025, rs2605593, rs3749387, rs3771382, rs3790731, rs3943933, rs411833, rs4360494, rs4419475, rs4677150, rs4769763, rs4780885, rs4837613, rs4887925, rs537508, rs597539, rs607472, rs6441042, rs6461943, rs6597653, rs6685593, rs67736411, rs6888042, rs6899218, rs7020201, rs7652177, rs7709645, rs7916441, rs7942152, rs874356, rs912690, rs9379130, rs9475170, rs9678029
#Harmonising Whole body fat-free mass (3h3wgh) and outcome (EPi0B6)
#Removing the following SNPs for incompatible alleles:
#  rs12581771, rs146643467, rs34898535, rs35228793, rs3740591
#Removing the following SNPs for being palindromic with intermediate allele frequencies:
#  rs1024853, rs10792, rs10887578, rs1093019, rs11234242, rs1147345, rs1188178, rs12572775, rs12631813, rs12897618, rs12927987, rs13047416, rs13264909, rs1454687, rs146728296, rs1561298, rs1652946, rs2238435, rs2284746, rs2305918, rs254025, rs2605593, rs3749387, rs3771382, rs3790731, rs3943933, rs411833, rs4360494, rs4419475, rs4677150, rs4769763, rs4780885, rs4837613, rs4887925, rs537508, rs597539, rs607472, rs6441042, rs6461943, rs6597653, rs6685593, rs67736411, rs6888042, rs6899218, rs7020201, rs7652177, rs7709645, rs7916441, rs7942152, rs874356, rs912690, rs9379130, rs9475170, rs9678029


#Non-overlapping instruments for univariable analyses

fm_snps <- unique(fm_instruments_ukb$SNP)
ffm_snps <- unique(ffm_instruments_ukb$SNP)
overlap <- intersect(ffm_snps, fm_snps)

nooverlapffm <- subset(ffm_instruments_ukb, !(ffm_instruments_ukb$SNP %in% overlap))


nooverlapfm <- subset(fm_instruments_ukb, !(fm_instruments_ukb$SNP %in% overlap))


#loading outcome data for T2D to use for R2 assessment

outcome_snps <- data.table::fread("https://storage.googleapis.com/finngen-public-data-r8/summary_stats/finngen_R8_T2D_WIDE.gz")


outcome <- format_data(
  outcome_snps,
  type = "outcome",
  snps = nooverlapfm$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)


length(unique(outcome$SNP)) #PRES 531 out of 531 in finngen
dat <- harmonise_data(nooverlapfm, outcome)


mr_steiger3 <- function(r_exp, r_out, n_exp, n_out){
  requireNamespace("psych", quietly=TRUE)
  index <- any(is.na(r_exp)) | any(is.na(r_out)) | any(is.na(n_exp)) | any(is.na(n_out))
  n_exp <- n_exp[!index]
  n_out <- n_out[!index]
  
  r_exp <- sqrt(sum(r_exp^2))
  r_out <- sqrt(sum(r_out^2))
  
  rtest <- psych::r.test(n = mean(n_exp), n2 = mean(n_out), r12 = r_exp, r34 = r_out)
  l <- list(
    r2_exp = r_exp^2, 
    r2_out = r_out^2, 
    correct_causal_direction = r_exp > r_out, 
    steiger_test = pnorm(-abs(rtest[["z"]]))*2
  )
  return(l)
}


mro <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))


dat$r_exp <- get_r_from_bsen(
  dat$beta.exposure,
  dat$se.exposure,
  dat$samplesize.exposure)



dat$r_out <- get_r_from_lor(
  dat$beta.outcome,
  dat$eaf.outcome,
  ncase=38657,
  ncontrol=310131,
  prevalence = 38657/(377277),
  model = "logit",
  correction = FALSE)

dat$samplesize.outcome <- 38657+310131

#dat$r_out <- get_r_from_lor(
#  dat$beta.outcome,
#  dat$eaf.outcome,
#  ncase=33043,
#  ncontrol=284971,
#  prevalence = 33043/(342499),
#  model = "logit",
#  correction = FALSE)

#dat$samplesize.outcome <- 33043+284971

direction <- mr_steiger3(dat$r_exp, dat$r_out, dat$samplesize.exposure,
                         dat$samplesize.outcome)


rsq <- direction$r2_exp #PRES 0.0671910147050817


outcome2 <- format_data(
  outcome_snps,
  type = "outcome",
  snps = nooverlapffm$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

dat2 <- harmonise_data(nooverlapffm, outcome2)


mro2 <- mr(dat2, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))


dat2$r_exp <- get_r_from_bsen(
  dat2$beta.exposure,
  dat2$se.exposure,
  dat2$samplesize.exposure)

dat2$r_out <- get_r_from_lor(
  dat2$beta.outcome,
  dat2$eaf.outcome,
  ncase=38657,
  ncontrol=310131,
  prevalence = 38657/(377277),
  model = "logit",
  correction = FALSE)

dat2$samplesize.outcome <- 38657+310131

#dat2$r_out <- get_r_from_lor(
#  dat2$beta.outcome,
#  dat2$eaf.outcome,
#  ncase=33043,
#  ncontrol=284971,
#  prevalence = 33043/(342499),
#  model = "logit",
#  correction = FALSE)

#dat2$samplesize.outcome <- 33043+284971

direction2 <- mr_steiger3(dat2$r_exp, dat2$r_out, dat2$samplesize.exposure,
                          dat2$samplesize.outcome)


rsq2 <- direction2$r2_exp #FFM 0.149336688831858

multi_fm <- subset(exposure_dat, !grepl("fat-free", exposure_dat$exposure) &
                     exposure_dat$pval.exposure < 5e-08)

multi_ffm <- subset(exposure_dat, grepl("DZHnnX", exposure_dat$id.exposure) &
                      exposure_dat$pval.exposure < 5e-08)

outcome3 <- format_data(
  outcome_snps,
  type = "outcome",
  snps = multi_fm$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

dat3 <- harmonise_data(multi_fm, outcome3)


mro3 <- mr(dat3, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))


dat3$samplesize.exposure <- 400154
dat3$r_exp <- get_r_from_bsen(
  dat3$beta.exposure,
  dat3$se.exposure,
  dat3$samplesize.exposure)

dat3$r_out <- get_r_from_lor(
  dat3$beta.outcome,
  dat3$eaf.outcome,
  ncase=38657,
  ncontrol=310131,
  prevalence = 38657/(377277),
  model = "logit",
  correction = FALSE)

#dat3$r_out <- get_r_from_lor(
#  dat3$beta.outcome,
#  dat3$eaf.outcome,
#  ncase=33043,
#  ncontrol=284971,
#  prevalence = 33043/(342499),
#  model = "logit",
#  correction = FALSE)

#dat3$samplesize.outcome <- 33043+284971

dat3$samplesize.outcome <- 38657+310131

direction3 <- mr_steiger3(dat3$r_exp, dat3$r_out, dat3$samplesize.exposure,
                          dat3$samplesize.outcome)


rsq3 <- direction3$r2_exp #FM Multi 0.06880644

outcome4 <- format_data(
  outcome_snps,
  type = "outcome",
  snps = multi_ffm$SNP,
  header = TRUE,
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  eaf_col = "af_alt",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  pval_col = "pval",
  gene_col = "nearest_genes",
  min_pval = 1e-200,
  info_col = "info",
  chr_col = "chrom",
  pos_col = "pos",
  log_pval = FALSE)

dat4 <- harmonise_data(multi_ffm, outcome4)


mro4 <- mr(dat4, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))

dat4$samplesize.exposure <- 400154
dat4$r_exp <- get_r_from_bsen(
  dat4$beta.exposure,
  dat4$se.exposure,
  dat4$samplesize.exposure)

dat4$r_out <- get_r_from_lor(
  dat4$beta.outcome,
  dat4$eaf.outcome,
  ncase=38657,
  ncontrol=310131,
  prevalence = 38657/(377277),
  model = "logit",
  correction = FALSE)

dat4$samplesize.outcome <- 38657+310131

#dat4$r_out <- get_r_from_lor(
#  dat4$beta.outcome,
#  dat4$eaf.outcome,
#  ncase=33043,
#  ncontrol=284971,
#  prevalence = 33043/(342499),
#  model = "logit",
#  correction = FALSE)

#dat4$samplesize.outcome <- 33043+284971

direction4 <- mr_steiger3(dat4$r_exp, dat4$r_out, dat4$samplesize.exposure,
                          dat4$samplesize.outcome)


rsq4 <- direction4$r2_exp #FFM Multi 0.1620818


dat$Fstat <- (dat$beta.exposure^2)/(dat$se.exposure^2)
mean(dat$Fstat) #50.64

dat3$Fstat <- (dat3$beta.exposure^2)/(dat3$se.exposure^2)
mean(dat3$Fstat) #55.63867

dat2$Fstat <- (dat2$beta.exposure^2)/(dat2$se.exposure^2)
mean(dat2$Fstat) #60.50009

dat4$Fstat <- (dat4$beta.exposure^2)/(dat4$se.exposure^2)
mean(dat4$Fstat) #65.53491


#Assessing associations with common confounders and height


exercise_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.moderate_37.regenie.merged.txt")
exercise_regenie <- 
  exercise_regenie %>% mutate(P = (10^(-LOG10P)))
fm_exercise <- merge(nooverlapfm, exercise_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_exercise <- merge(nooverlapffm, exercise_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)


height_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.height_37.regenie.merged.txt")
height_regenie <- 
  height_regenie %>% mutate(P = (10^(-LOG10P)))
fm_height <- merge(nooverlapfm, height_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_height <- merge(nooverlapffm, height_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)


townsend_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.townsend_37.regenie.merged.txt")
townsend_regenie <- 
  townsend_regenie %>% mutate(P = (10^(-LOG10P)))
fm_townsend <- merge(nooverlapfm, townsend_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_townsend <- merge(nooverlapffm, townsend_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)

qualifications_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.qualifications_37.regenie.merged.txt")
qualifications_regenie <- 
  qualifications_regenie %>% mutate(P = (10^(-LOG10P)))
fm_qualifications <- merge(nooverlapfm, qualifications_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_qualifications <- merge(nooverlapffm, qualifications_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)

ever_smoked_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.ever_smoked_37.regenie.merged.txt")
ever_smoked_regenie <- 
  ever_smoked_regenie %>% mutate(P = (10^(-LOG10P)))
fm_ever_smoked <- merge(nooverlapfm, ever_smoked_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_ever_smoked <- merge(nooverlapffm, ever_smoked_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)

alcohol_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.alcohol_37.regenie.merged.txt")
alcohol_regenie <- 
  alcohol_regenie %>% mutate(P = (10^(-LOG10P)))
fm_alcohol <- merge(nooverlapfm, alcohol_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_alcohol <- merge(nooverlapffm, alcohol_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)


fm_with_ffm <- merge(nooverlapfm, fat_free_mass_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)
ffm_with_fm <- merge(nooverlapffm, fat_mass_regenie, by.x="SNP", by.y = "ID", all.x = TRUE)



#fat_free_mass_regenie <- read.delim("path_to_merged_confounder_regenie_output/assoc.fat_free_mass.regenie.merged.txt")

fm_confounders <- list(fm_with_ffm, fm_height, fm_townsend, fm_qualifications, fm_exercise, fm_ever_smoked, fm_alcohol)
ffm_confounders <- list(ffm_with_fm, ffm_height, ffm_townsend, ffm_qualifications, ffm_exercise, ffm_ever_smoked, ffm_alcohol)


pleiotropy_fm <- data.frame(
  trait = character(), 
  beta = numeric(), 
  se = numeric(), 
  pval = numeric()
)

traits <- c("FFM", "Height", "Townsend index", "College / University degree", "Exercise", "Ever smoked", "Alcohol intake frequency")

for(i in 1:7){try({
  dat <- fm_confounders[[i]]
  dat <- data.frame(dvals = dat$BETA.y,
                    stderrs  = dat$SE.y)
  test <- rma(dvals, sei=stderrs, data=dat)
  
  pleiotropy_fm <- rbind(pleiotropy_fm, data.frame(
    trait = traits[i], 
    beta = test$beta, 
    se = test$se, 
    pval = test$pval
  ))
  
})
}

write.csv(pleiotropy_fm, 'pleiotropy_fm_01.csv')


format_numeric <- function(x) {
  ifelse(abs(x) >= 0.001 & abs(x) < 1, round(x, 3), format(x, scientific = TRUE, digits=3))
}

pleiotropy_fm$beta <- format_numeric(as.numeric(pleiotropy_fm$beta))
pleiotropy_fm$se <- format_numeric(as.numeric(pleiotropy_fm$se))
pleiotropy_fm$pval <- format_numeric(as.numeric(pleiotropy_fm$pval))

set_flextable_defaults(
  font.size = 7, theme_fun = theme_vanilla,
  padding = 2,
  font.family = "serif",
  background.color = "white")

pleiotropy_fm <- flextable(
  data = pleiotropy_fm) %>%
  autofit()

pleiotropy_fm <- theme_zebra(pleiotropy_fm)

pleiotropy_fm <- colformat_num(x = pleiotropy_fm,
                               big.mark="", decimal.mark = ".",
                               na_str = "N/A")
pleiotropy_fm <- align_nottext_col(pleiotropy_fm, align="left")

pleiotropy_fm <- width(pleiotropy_fm, width = 0.6)
pleiotropy_fm <- height(pleiotropy_fm, height = 0.4)

save_as_docx(path="pleiotropy_fm_01.docx", pleiotropy_fm)

pleiotropy_ffm <- data.frame(
  trait = character(), 
  beta = numeric(), 
  se = numeric(), 
  pval = numeric()
)

pleiotropy_ffm <- data.frame(
  trait = character(), 
  beta = numeric(), 
  se = numeric(), 
  pval = numeric()
)


traits <- c("FM", "Height", "Townsend index", "College / University degree", "Exercise", "Ever smoked", "Alcohol intake frequency")

for(i in 1:7){try({
  dat <- ffm_confounders[[i]]
  dat <- data.frame(dvals = dat$BETA.y,
                    stderrs  = dat$SE.y)
  test <- rma(dvals, sei=stderrs, data=dat)
  
  pleiotropy_ffm <- rbind(pleiotropy_ffm, data.frame(
    trait = traits[i], 
    beta = test$beta, 
    se = test$se, 
    pval = test$pval
  ))
  
})
}


write.csv(pleiotropy_ffm, 'pleiotropy_ffm_01.csv')



pleiotropy_ffm$beta <- format_numeric(as.numeric(pleiotropy_ffm$beta))
pleiotropy_ffm$se <- format_numeric(as.numeric(pleiotropy_ffm$se))
pleiotropy_ffm$pval <- format_numeric(as.numeric(pleiotropy_ffm$pval))

set_flextable_defaults(
  font.size = 7, theme_fun = theme_vanilla,
  padding = 2,
  font.family = "serif",
  background.color = "white")

pleiotropy_ffm <- flextable(
  data = pleiotropy_ffm) %>%
  autofit()

pleiotropy_ffm <- theme_zebra(pleiotropy_ffm)

pleiotropy_ffm <- colformat_num(x = pleiotropy_ffm,
                                big.mark="", decimal.mark = ".",
                                na_str = "N/A")
pleiotropy_ffm <- align_nottext_col(pleiotropy_ffm, align="left")

pleiotropy_ffm <- width(pleiotropy_ffm, width = 0.6)
pleiotropy_ffm <- height(pleiotropy_ffm, height = 0.4)

save_as_docx(path="pleiotropy_ffm_01.docx", pleiotropy_ffm)


