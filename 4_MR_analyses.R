

#defining to assess R2 and the MR-Steiger test

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


mr <- data.frame()
mr_nooverlap <- data.frame()
mvmr <- data.frame()
mr_nooverlap_ffm <- data.frame()
mr_ffm <- data.frame()

ids <- unique(finn$path_https)


for(i in 1:821){try({
  outcome_snps <- data.table::fread(ids[i])
  
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(fm_instruments_ukb, outcome)
  
  fm_dat <- dat
  
  mr2 <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))
  intercept <- data.frame(mr_pleiotropy_test(dat))
  intercept$method <- "MR Egger intercept"
  intercept$nsnp <- NA
  names(intercept)[names(intercept) == "egger_intercept"] <- "b"
  mr2 <- rbind(mr2, intercept)
  
  dat$r_exp <- get_r_from_bsen(
    dat$beta.exposure,
    dat$se.exposure,
    dat$samplesize.exposure)
  
  dat$r_out <- get_r_from_lor(
    dat$beta.outcome,
    dat$eaf.outcome,
    ncase=finn$num_cases[i],
    ncontrol=finn$num_controls[i],
    prevalence = finn$num_cases[i]/(342499),
    model = "logit",
    correction = FALSE)
  
  dat$samplesize.outcome <- finn$num_cases[i]+finn$num_controls[i]
  
  direction <- mr_steiger3(dat$r_exp, dat$r_out, dat$samplesize.exposure,
                           dat$samplesize.outcome)
  
  mr2$direction <- direction$correct_causal_direction
  mr2$direction_p <- direction$steiger_test
  mr <- rbind(mr, mr2)
  
  n_exposure <- dat$samplesize.exposure
  n_outcome <- finn$num_cases[i]+finn$num_controls[i]
  
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(nooverlapfm, outcome)
  mro <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))
  intercept <- data.frame(mr_pleiotropy_test(dat))
  intercept$method <- "MR Egger intercept"
  intercept$nsnp <- NA
  names(intercept)[names(intercept) == "egger_intercept"] <- "b"
  mro <- rbind(mro, intercept)
  
  dat$r_exp <- get_r_from_bsen(
    dat$beta.exposure,
    dat$se.exposure,
    dat$samplesize.exposure)
  
  dat$r_out <- get_r_from_lor(
    dat$beta.outcome,
    dat$eaf.outcome,
    ncase=finn$num_cases[i],
    ncontrol=finn$num_controls[i],
    prevalence = finn$num_cases[i]/(342499),
    model = "logit",
    correction = FALSE)
  
  dat$samplesize.outcome <- finn$num_cases[i]+finn$num_controls[i]
  direction <- mr_steiger3(dat$r_exp, dat$r_out, dat$samplesize.exposure,
                           dat$samplesize.outcome)
  
  mro$direction <- direction$correct_causal_direction
  mro$direction_p <- direction$steiger_test
  mr_nooverlap <- rbind(mr_nooverlap, mro)
  
  
  outcome_mvmr <- format_data(
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
  
  outcome_mvmr$id.outcome  <- finn$name[i]
  
  
  exposure_dat$samplesize.exposure <- 400154
  
  mvdat <- mv_harmonise_data(exposure_dat, outcome_mvmr, harmonise_strictness = 2)
  mvmr2 <- mv_ivw(mvdat, pval_threshold = 5e-08)
  mvmr3 <- mvmr2$result
  
  mvdat_dir <- as.data.frame(mvdat$exposure_beta[,2])
  mvdat_dir[, 2] <- mvdat$exposure_se[,2]
  mvdat_dir[, 3] <- mvdat$exposure_pval[,2]
  mvdat_dir[, 4] <- n_exposure[1]
  mvdat_dir[, 5] <- mvdat$outcome_beta
  mvdat_dir[, 6] <- mvdat$outcome_se
  mvdat_dir[, 7] <- n_outcome[1]
  
  names(mvdat_dir) <- c("beta.exposure", "se.exposure", "pval.exposure",
                        "samplesize.exposure", "beta.outcome", "se.outcome",
                        "samplesize.outcome")
  
  mvdat_dir <- subset(mvdat_dir, mvdat_dir$pval.exposure < 5e-08)
  
  mvdat_dir$r_exp <- get_r_from_bsen(
    mvdat_dir$beta.exposure,
    mvdat_dir$se.exposure,
    mvdat_dir$samplesize.exposure)
  
  
  mvdat_dir$SNP <- rownames(mvdat_dir)
  
  
  outcome_ffm <- format_data(
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
  
  
  ffm_dat <- harmonise_data(ffm_instruments_ukb, outcome_ffm)
  ffm_dat <- ffm_dat[, c("SNP", "eaf.outcome")]
  ffm_dat <- rbind(ffm_dat, fm_dat[, c("SNP", "eaf.outcome")])
  ffm_dat <- unique(ffm_dat)
  mvdat_dir <- merge(mvdat_dir, ffm_dat, by = "SNP", all.x=TRUE)
  
  mvdat_dir$r_out <- get_r_from_lor(
    mvdat_dir$beta.outcome,
    mvdat_dir$eaf.outcome,
    ncase=finn$num_cases[i],
    ncontrol=finn$num_controls[i],
    prevalence = finn$num_cases[i]/(342499),
    model = "logit",
    correction = FALSE)
  
  mvdat_dir$samplesize.outcome <- finn$num_cases[i]+finn$num_controls[i]
  
  direction <- mr_steiger3(mvdat_dir$r_exp, mvdat_dir$r_out, 
                           mvdat_dir$samplesize.exposure,
                           mvdat_dir$samplesize.outcome)
  
  mvmr3$direction <- direction$correct_causal_direction
  mvmr3$direction_p <- direction$steiger_test
  
  mvmr <- rbind(mvmr, mvmr3)
  
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(ffm_instruments_ukb, outcome)
  
  fm_dat <- dat
  
  mr2 <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))
  intercept <- data.frame(mr_pleiotropy_test(dat))
  intercept$method <- "MR Egger intercept"
  intercept$nsnp <- NA
  names(intercept)[names(intercept) == "egger_intercept"] <- "b"
  mr2 <- rbind(mr2, intercept)
  
  dat$r_exp <- get_r_from_bsen(
    dat$beta.exposure,
    dat$se.exposure,
    dat$samplesize.exposure)
  
  dat$r_out <- get_r_from_lor(
    dat$beta.outcome,
    dat$eaf.outcome,
    ncase=finn$num_cases[i],
    ncontrol=finn$num_controls[i],
    prevalence = finn$num_cases[i]/(342499),
    model = "logit",
    correction = FALSE)
  
  dat$samplesize.outcome <- finn$num_cases[i]+finn$num_controls[i]
  
  direction <- mr_steiger3(dat$r_exp, dat$r_out, dat$samplesize.exposure,
                           dat$samplesize.outcome)
  
  mr2$direction <- direction$correct_causal_direction
  mr2$direction_p <- direction$steiger_test
  mr_ffm <- rbind(mr_ffm, mr2)
  
  n_exposure <- dat$samplesize.exposure
  n_outcome <- finn$num_cases[i]+finn$num_controls[i]
  
  outcome <- format_data(
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(nooverlapffm, outcome)
  mro <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))
  intercept <- data.frame(mr_pleiotropy_test(dat))
  intercept$method <- "MR Egger intercept"
  intercept$nsnp <- NA
  names(intercept)[names(intercept) == "egger_intercept"] <- "b"
  mro <- rbind(mro, intercept)
  
  dat$r_exp <- get_r_from_bsen(
    dat$beta.exposure,
    dat$se.exposure,
    dat$samplesize.exposure)
  
  dat$r_out <- get_r_from_lor(
    dat$beta.outcome,
    dat$eaf.outcome,
    ncase=finn$num_cases[i],
    ncontrol=finn$num_controls[i],
    prevalence = finn$num_cases[i]/(342499),
    model = "logit",
    correction = FALSE)
  
  dat$samplesize.outcome <- finn$num_cases[i]+finn$num_controls[i]
  direction <- mr_steiger3(dat$r_exp, dat$r_out, dat$samplesize.exposure,
                           dat$samplesize.outcome)
  
  mro$direction <- direction$correct_causal_direction
  mro$direction_p <- direction$steiger_test
  mr_nooverlap_ffm <- rbind(mr_nooverlap_ffm, mro)
  
  write.csv(mr_ffm, 'ukb_ffm_finngen_r8_mr_01.csv')
  write.csv(mr_nooverlap_ffm, 'ukb_ffm_finngen_r8_mr_nooverlap_01.csv')
  write.csv(mr, 'ukb_finngen_r8_mr_01.csv')
  write.csv(mr_nooverlap, 'ukb_finngen_r8_mr_nooverlap_01.csv')
  write.csv(mvmr, 'ukb_finngen_r8_mvmr_01.csv')
  
  print(i/821)
  
})
}


#Contamination mixture analyses

mr_contmix <- data.frame()
mr_contmix_ffm <- data.frame()

ids <- unique(finn$path_https)


for(i in 1:821){try({
  
  outcome_snps <- data.table::fread(ids[i])
  
  
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(nooverlapfm, outcome)
  dat_contmix <- dat_to_MRInput(dat, get_correlations = FALSE, pop = "EUR")
  dat_contmix <- dat_contmix[["Whole body fat mass.outcome"]]
  mro <- MendelianRandomization::mr_conmix(dat_contmix, psi = 0, CIMin = NA, CIMax = NA,
                                           CIStep =0.01, alpha = 0.05)
  
  mro <- data.frame(
    Exposure = mro@Exposure,
    Outcome = mro@Outcome,
    Psi = mro@Psi,
    Estimate = mro@Estimate,
    CIRange = I(list(mro@CIRange)),
    CILower = mro@CILower,
    CIUpper = mro@CIUpper,
    CIMin = mro@CIMin,
    CIMax = mro@CIMax,
    CIStep = mro@CIStep,
    Valid = I(list(mro@Valid)),
    ValidSNPs = I(list(mro@ValidSNPs)),
    Pvalue = mro@Pvalue,
    Alpha = mro@Alpha,
    SNPs = mro@SNPs)
  
  
  mr_contmix <- rbind(mr_contmix, mro)
  # Convert list-type columns to string representation
  mr_contmix$CIRange <- sapply(mr_contmix$CIRange, paste, collapse = ",")
  mr_contmix$Valid <- sapply(mr_contmix$Valid, paste, collapse = ",")
  mr_contmix$ValidSNPs <- sapply(mr_contmix$ValidSNPs, paste, collapse = ",")
  
  # Write the data frame to an Excel file
  openxlsx::write.xlsx(mr_contmix, file = "mr_contmix_all_finngen_r8_mr_01.xlsx")
  
  
  
  outcome <- format_data(
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
  
  outcome$id.outcome <- finn$name[i]
  
  dat <- harmonise_data(nooverlapffm, outcome)
  dat_contmix <- dat_to_MRInput(dat, get_correlations = FALSE, pop = "EUR")
  dat_contmix <- dat_contmix[["Whole body fat-free mass.outcome"]]
  mro <- MendelianRandomization::mr_conmix(dat_contmix, psi = 0, CIMin = NA, CIMax = NA,
                                           CIStep =0.01, alpha = 0.05)
  
  mro <- data.frame(
    Exposure = mro@Exposure,
    Outcome = mro@Outcome,
    Psi = mro@Psi,
    Estimate = mro@Estimate,
    CIRange = I(list(mro@CIRange)),
    CILower = mro@CILower,
    CIUpper = mro@CIUpper,
    CIMin = mro@CIMin,
    CIMax = mro@CIMax,
    CIStep = mro@CIStep,
    Valid = I(list(mro@Valid)),
    ValidSNPs = I(list(mro@ValidSNPs)),
    Pvalue = mro@Pvalue,
    Alpha = mro@Alpha,
    SNPs = mro@SNPs)
  
  mr_contmix_ffm <- rbind(mr_contmix_ffm, mro)
  # Convert list-type columns to string representation
  mr_contmix_ffm$CIRange <- sapply(mr_contmix_ffm$CIRange, paste, collapse = ",")
  mr_contmix_ffm$Valid <- sapply(mr_contmix_ffm$Valid, paste, collapse = ",")
  mr_contmix_ffm$ValidSNPs <- sapply(mr_contmix_ffm$ValidSNPs, paste, collapse = ",")
  
  # Write the data frame to an Excel file
  openxlsx::write.xlsx(mr_contmix_ffm, file = "mr_contmix_all_ffm_finngen_r8_mr_01.xlsx")
  
  
  print(i/821)
  
})
}

assign_ids <- finn$name
mr_contmix_ffm$Outcome <- assign_ids
mr_contmix$Outcome <- assign_ids
openxlsx::write.xlsx(mr_contmix_ffm, file = "mr_contmix_all_ffm_finngen_r8_mr_01.xlsx")
openxlsx::write.xlsx(mr_contmix, file = "mr_contmix_all_finngen_r8_mr_01.xlsx")

mro_fm <- data.frame()
mro_ffm <- data.frame()

#myo <- subset(finn, startsWith(finn$name, "Myocardial"))

ids <- unique(finn$path_https)
#ids <- unique(myo$path_https)

#finn <- subset(finn, finn$name %in% c("Heart failure and bmi 25plus", "Obesity", "Localized adiposity", "Obesity related asthma", "Extreme obesity with alveolar hypoventilation"))

#Add heterogenity statistic

for(i in 1:821){try({
  outcome_snps <- data.table::fread(ids[i])
  
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(nooverlapfm, outcome)
  #mro <- mr(dat, method_list=c("mr_ivw", "mr_weighted_median", "mr_weighted_mode", "mr_egger_regression"))
  mro_fm_temp <- mr_heterogeneity(
    dat,
    parameters = default_parameters(),
    method_list = subset(mr_method_list(), heterogeneity_test & use_by_default)$obj
  )
  
  mro_fm <- rbind(mro_fm, mro_fm_temp)
  
  outcome <- format_data(
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
  
  outcome$id.outcome <- finn$name[i]
  
  
  dat <- harmonise_data(nooverlapffm, outcome)
  mro_ffm_temp <- mr_heterogeneity(
    dat,
    parameters = default_parameters(),
    method_list = subset(mr_method_list(), heterogeneity_test & use_by_default)$obj
  )
  
  mro_ffm <- rbind(mro_ffm, mro_ffm_temp)
  
  write.csv(mro_fm, 'heterogeneity_fm_finngen_r8_mr_nooverlap_01.csv')
  write.csv(mro_ffm, 'heterogeneity_ffm_finngen_r8_mr_nooverlap_01.csv')
  print(i/821)
  
})
}


