#install.packages("dplyr")
library(dplyr)
#install.packages("readxl")
library(readxl)
#remotes::install_github("MRCIEU/TwoSampleMR")
library(TwoSampleMR)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("stringr")
library(stringr)
#install.packages("wesanderson")
library(wesanderson)
#install.packages("foreach")
library(foreach)
#install.packages("doParallel")
library(doParallel)
#install.packages("R.utils")
library(R.utils)
#remotes::install_github("neilstats/ckbplotr")
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

#ao <- available_outcomes()
library(ieugwasr)
#library(plyr)


R8_release <- read.delim("path_to_finngen_manifest_file/summary_stats_R8_manifest.tsv")

unique(R8_release$category)

nrow(subset(R8_release, is.na(R8_release$category)))

#R8_release <- subset(R8_release, R8_release$category=="IX Diseases of the circulatory system (I9_)")

finngen_R8_endpoint_core_noncore_1_0 <- read_excel("path_to_finngen_outcome_overview/FINNGEN_CORE_AND_NONCORE_ENDPOINTS_AND_CONTROLS_DF8_OK (1).xlsx")

finn <- merge(R8_release, finngen_R8_endpoint_core_noncore_1_0, by.x ="phenocode", by.y ="NAME", all.x=T)

unique(finn$category)
original_strings <- c(
  "I Certain infectious and parasitic diseases (AB1_)",
  "II Neoplasms, from cancer register (ICD-O-3)",
  "II Neoplasms from hospital discharges (CD2_)",
  "III Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism (D3_)",
  "IV Endocrine, nutritional and metabolic diseases (E4_)",
  "V Mental and behavioural disorders (F5_)",
  "VI Diseases of the nervous system (G6_)",
  "VII Diseases of the eye and adnexa (H7_)",
  "VIII Diseases of the ear and mastoid process (H8_)",
  "IX Diseases of the circulatory system (I9_)",
  "X Diseases of the respiratory system (J10_)",
  "XI Diseases of the digestive system (K11_)",
  "XII Diseases of the skin and subcutaneous tissue (L12_)",
  "XIII Diseases of the musculoskeletal system and connective tissue (M13_)",
  "XIV Diseases of the genitourinary system (N14_)",
  "XV Pregnancy, childbirth and the puerperium (O15_)",
  "XVI Certain conditions originating in the perinatal period (P16_)",
  "XVII Congenital malformations, deformations and chromosomal abnormalities (Q17)",
  "XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R18_)",
  "XIX Injury, poisoning and certain other consequences of external causes (ST19_)",
  "XX External causes of morbidity and mortality (VWXY20_)",
  "XXI Factors influencing health status and contact with health services (Z21_)"
)

new_strings <- c(
  "Infectious and parasitic diseases",
  "Neoplasms",
  "Neoplasms",
  "Diseases of blood and blood-forming organs and immune disorders",
  "Endocrine, nutritional and metabolic diseases",
  "Mental and behavioural disorders",
  "Nervous system",
  "Eye and adnexa",
  "Ear and mastoid process",
  "Circulatory system",
  "Respiratory system",
  "Digestive system",
  "Skin and subcutaneous tissue",
  "Musculoskeletal system and connective tissue",
  "Genitourinary system",
  "Pregnancy, childbirth and the puerperium",
  "Conditions originating in the perinatal period",
  "Congenital malformations, deformations and chromosomal abnormalities",
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
  "Injury, poisoning and certain other consequences of external causes",
  "External causes of morbidity and mortality",
  "Factors influencing health status and contact with health services"
)

finn$category <- new_strings[match(finn$category, original_strings)]

unique(finn$category)

nrow(subset(finn, !is.na(finn$HD_ICD_10)))
nrow(subset(finn, !is.na(finn$LEVEL)))


finn$ICD <- ifelse(!is.na(finn$HD_ICD_10), finn$HD_ICD_10, ifelse(!is.na(finn$COD_ICD_10), finn$COD_ICD_10, finn$OUTPAT_ICD))

finn$ICD <- ifelse(grepl("Cerebrovascular", finn$name), "I6[1-2],I6[3-4],I6[60-74,76,77,681-682]", finn$ICD)

finn$ICD <- ifelse(grepl("Diseases of arteries", finn$name), "I70,I71[00-01,1-6,8,9],I72,I74", finn$ICD)

finn$ICD <- ifelse(grepl("STROKE", finn$name), "I6[0-4],G45", finn$ICD)

finn$ICD <- ifelse(grepl("Cardiac arrhythmias", finn$name), "I4[7-9]", finn$ICD)

finn$ICD <- ifelse(grepl("Valvular heart", finn$name), "I3[4-7]", finn$ICD)

finn$ICD <- ifelse(grepl("Hernia of abdominal wall", finn$name), "I42,I43,I45,I46", finn$ICD)

finn$ICD <- ifelse(grepl("Anaemias", finn$name), "D5[0-3],D55-D64", finn$ICD)

finn$ICD <- ifelse(grepl("Diabetic retinopathy", finn$name), "H36[00-04],H3609,H405,H431,E1.7,E1.3", finn$ICD)

finn$ICD <- ifelse(grepl("Renal failure", finn$name), "N1[7-9],Y841,Z992", finn$ICD)

#finn$ICD <- ifelse(grepl("Smoking", finn$name), "N1[7-9],Y84.1,Z99.2", finn$ICD)

finn$ICD <- ifelse(grepl("Sleep disorders", finn$name), "F51,G47", finn$ICD)

finn$ICD <- ifelse(grepl("Lower back pain or", finn$name), "M54[3-5]", finn$ICD)

finn$ICD <- ifelse(grepl("Dorsalgia", finn$name), "M54[0-6],M54[8-9]", finn$ICD)

finn$ICD <- ifelse(grepl("limb, back, neck, head", finn$name), "G50[0-1],H571,M255,M54[0-6],M54[8-9],M79[0-3],M79[6-7],K076,R10,R5[1-2]", finn$ICD)

finn$ICD <- ifelse(grepl("Malignant neoplasm", finn$name), "C00-C26,C30-C41,C43-C58,C60-C86,C88,C90-C97", finn$ICD)

finn$ICD <- ifelse(grepl("Nontoxic goitre", finn$name), "E04[0-2],E04[8-9]", finn$ICD)

finn_before_exclusions <- finn

finn_before_exclusions$reason_if_excluded <- ifelse(
  !(!grepl("induced", finn_before_exclusions$name) | grepl("not induced", finn_before_exclusions$name)), "Attributed to external cause (alcohol or drug)", NA
)

finn <- subset(finn, (!grepl("induced", finn$name) | grepl("not induced", finn$name)))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("due to", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("due to", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("resulting from", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("resulting from", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("resulting in", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("resulting in", finn$name))


finn_before_exclusions$reason_if_excluded <- ifelse(
  !((!grepl(" in ", finn_before_exclusions$name)) 
    | grepl("in vitreous body", finn_before_exclusions$name)
    | grepl("Defects in the complement system", finn_before_exclusions$name)
    | grepl("Dysplastic lesion of unknown severity in the cervix uteri", finn_before_exclusions$name)
    | grepl("Excessive vomiting in pregnancy", finn_before_exclusions$name)
    | grepl("Haemorrhage in early pregnancy", finn_before_exclusions$name)
    | grepl("Hypertrophy of breast in women and men", finn_before_exclusions$name)
    | grepl("Infections of genitourinary tract in pregnancy", finn_before_exclusions$name)
    | grepl("Loose body in joint", finn_before_exclusions$name)
    | grepl("Pain in joint", finn_before_exclusions$name)
    | grepl("Pain in limb", finn_before_exclusions$name)
    | grepl("Pain in thoracic spine", finn_before_exclusions$name)
    | grepl("Venous complications and haemorrhoids in pregnancy", finn_before_exclusions$name)
    | grepl("Venous complications and haemorrhoids in the puerperium", finn_before_exclusions$name)), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)


finn <- subset(finn, (!grepl(" in ", finn$name)) 
               | grepl("in vitreous body", finn$name)
               | grepl("Defects in the complement system", finn$name)
               | grepl("Dysplastic lesion of unknown severity in the cervix uteri", finn$name)
               | grepl("Excessive vomiting in pregnancy", finn$name)
               | grepl("Haemorrhage in early pregnancy", finn$name)
               | grepl("Hypertrophy of breast in women and men", finn$name)
               | grepl("Infections of genitourinary tract in pregnancy", finn$name)
               | grepl("Loose body in joint", finn$name)
               | grepl("Pain in joint", finn$name)
               | grepl("Pain in limb", finn$name)
               | grepl("Pain in thoracic spine", finn$name)
               | grepl("Venous complications and haemorrhoids in pregnancy", finn$name)
               | grepl("Venous complications and haemorrhoids in the puerperium", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("secondary", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("secondary", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Secondary", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("Secondary", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Sequelae", finn_before_exclusions$name), "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("Sequelae", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("not elsewhere classified", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("not elsewhere classified", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("no elsewhere classified", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("no elsewhere classified", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("not classified elsewhere", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("not classified elsewhere", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("not otherwise specified", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("not otherwise specified", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("not specified as", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("not specified as", finn$name))


finn_before_exclusions$reason_if_excluded <- ifelse(
  !((!grepl("unspecified", finn_before_exclusions$name) & !grepl("Unspecified", finn_before_exclusions$name))
    | grepl("Female infertility, cervigal, vaginal, other or unspecified origin", finn_before_exclusions$name)
    | grepl("Inflammation of lacrimal passages", finn_before_exclusions$name)
    | grepl("Suppurative and unspecified otitis media", finn_before_exclusions$name)), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, ((!grepl("unspecified", finn$name) & !grepl("Unspecified", finn$name))
                      | grepl("Female infertility, cervigal, vaginal, other or unspecified origin", finn$name)
                      | grepl("Inflammation of lacrimal passages", finn$name)
                      | grepl("Suppurative and unspecified otitis media", finn$name)))


finn_before_exclusions$reason_if_excluded <- ifelse(
  !((!grepl(" NAS", finn_before_exclusions$name)) 
    | grepl("naso", finn_before_exclusions$name)
    | grepl("nasa", finn_before_exclusions$name)
    | grepl("Naso", finn_before_exclusions$name)
    | grepl("Nasa", finn_before_exclusions$name)), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, (!grepl(" NAS", finn$name)) 
               | grepl("naso", finn$name)
               | grepl("nasa", finn$name)
               | grepl("Naso", finn$name)
               | grepl("Nasa", finn$name))


finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("diseases classified elsewhere", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("diseases classified elsewhere", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("ill-defined", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("ill-defined", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Ill-defined", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("Ill-defined", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("other and ill-defined", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("other and ill-defined", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Other", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !startsWith(finn$name, "Other"))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("of other", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("of other", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("for other", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("for other", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("other parts", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("other parts", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Other parts", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("Other parts", finn$name))


finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("as the cause of", finn_before_exclusions$name), "Ill-defined / miscellaneous outcomes grouped together", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("as the cause of", finn$name))

finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("medication", finn_before_exclusions$name), "Medication", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !grepl("medication", finn$name))


finn_before_exclusions$reason_if_excluded <- ifelse(
  (finn_before_exclusions$name=="Vascular dementia" & is.na(finn_before_exclusions$LEVEL)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(finn$name=="Vascular dementia" & is.na(finn$LEVEL)))

finn_before_exclusions$reason_if_excluded <- ifelse(
  (finn_before_exclusions$name=="Vascular diseases of the inrobustine" & is.na(finn_before_exclusions$LEVEL)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(finn$name=="Vascular diseases of the inrobustine" & is.na(finn$LEVEL)))

finn_before_exclusions$reason_if_excluded <- ifelse(
  (finn_before_exclusions$name=="Proliferative diabetic retinopathy" & finn_before_exclusions$num_cases<3000), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(finn$name=="Proliferative diabetic retinopathy" & finn$num_cases<3000))


finn_before_exclusions$reason_if_excluded <- ifelse(
  (grepl("Intrahepatic Cholestasis of Pregnancy", finn_before_exclusions$name) & grepl("more control exclusions", finn_before_exclusions$name) & !grepl("ICD", finn_before_exclusions$name)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(grepl("Intrahepatic Cholestasis of Pregnancy", finn$name) & grepl("more control exclusions", finn$name) & !grepl("ICD", finn$name))) 


finn_before_exclusions$reason_if_excluded <- ifelse(
  (grepl("Postpartum haemorrhage", finn_before_exclusions$name) & !grepl("O15_POSTPART_HEAMORRH_LB", finn_before_exclusions$phenocode)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(grepl("Postpartum haemorrhage", finn$name) & !grepl("O15_POSTPART_HEAMORRH_LB", finn$phenocode))) 


finn_before_exclusions$reason_if_excluded <- ifelse(
  (grepl("Intrahepatic Cholestasis of Pregnancy", finn_before_exclusions$name) & grepl("ICD", finn_before_exclusions$name) & !grepl("more control exclusions", finn_before_exclusions$name)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(grepl("Intrahepatic Cholestasis of Pregnancy", finn$name) & grepl("ICD", finn$name) & !grepl("more control exclusions", finn$name))) 



#Ill-defined / miscellaneous outcomes grouped together

#Not disease outcomes
exclusions <- c("Single delivery by caesarean section",
                "Medical abortion",
                "Failed attempted abortion",
                "Maternal care for (suspected) damage to fetus from alcohol",
                "Perineal laceration during delivery",
                "Single spontaneous delivery",
                "Multiple delivery",
                "Retained placenta and membranes, without haemorrhage",
                "Single delivery by forceps and vacuum extractor",
                "Slow fetal growth and fetal malnutrition",
                "Retention of urine",
                "Use of disulfiram, acamprosate or naltrexone",
                "Problems related to medical facilities and other health care",
                "Use of hypnotics and sedatives",
                "Use of antiglaucoma preparations and miotics",
                "Procreative management",
                "Hypothyroidism, drug reimbursement",
                "Invasive ventilation",
                "Tonsillectomy",
                "Appendectomy",
                "Cholecystectomy",
                "Non-invasive ventilation",
                "Any death") #edit?
finn <- subset(finn, !(finn$name %in% exclusions))

finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$name %in% exclusions, "Not disease outcomes", finn_before_exclusions$reason_if_excluded
)


#Attributed to a specific cause
exclusions <- c("Alcohol related diseases and deaths, all endpoints",
                "Any mental disorder, or suicide (or attempt), or psychic disorders complicating pregnancy, partum or puerperium or nerve system disorders (more control exclusions)",
                "Pre-existing hypertension complicating pregnancy, childbirth and the puerperium",
                "Pregnancy hypertension",
                "Gestational [pregnancy-induced] hypertension",
                "Gestational [pregnancy-induced] oedema and proteinuria without hypertension",
                "Gestational diabetes (for exclusion)",
                "Toxic effect of alcohol",
                "Toxic effect of ethanol",
                "Anaphylactic shock due to adverse effect of correct drug or medication properly administered",
                "Alcoholic cardiomyopathy",
                "Alcoholic gastritis",
                "Alcoholic liver disease",
                "Alcohol related diseases, tilastokeskus definition, death only",
                "Acute alcohol intoxication")

finn <- subset(finn, !(finn$name %in% exclusions))
finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$name %in% exclusions, "Attributed to a specific cause", finn_before_exclusions$reason_if_excluded
)


#CONGENITAL / DEVELOPMENTAL
exclusions <- c("Congenital iodine-deficiency syndrome/hypothyroidism",
                "Mental retardation (more control exclusions)",
                "Precocious central puberty",
                "Specific development disorders of speech and language",
                "Coagulation defects, purpura and other haemorrhagic conditions",
                "Speech and linguistic disorders (more control exclusions)",
                "Mental retardation (more control exclusions)",
                "Precocious central puberty",
                "Specific development disorders of speech and language",
                "Coagulation defects, purpura and other haemorrhagic conditions",
                "Speech and linguistic disorders (more control exclusions)",
                "Pervasive developmental disorders excl. Autism + Asperger",
                "Mixed specific developmental disorders",
                "Wide developmental disorders (more control exclusions)",
                "Autism spe (more control exclusions)"
)

finn <- subset(finn, !(finn$name %in% exclusions))
finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$name %in% exclusions, "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)


exclusions <- c("Intrahepatic Cholestasis of Pregnancy (ICP)  (more control exclusions)",
                "Intrahepatic Cholestasis of Pregnancy (ICP) , incl. ICD-8",
                "Benign neoplasm: Rectum/anal canal icd-9",
                "Dentinal caries",
                "Dental caries 2",
                "Smoking",
                "Alcohol dependence",
                "Dental pulpitis 3",
                "Coxarthrosis, primary, with hip surgery",
                "Primary coxarthrosis, bilateral",
                "Basal cell carcinoma, including avohilmo (controls excluding all cancers)",
                "Brain meningioma (controls excluding all cancers)",
                "Type 2 diabetes, definitions combined",
                "Malignant melanoma of skin (controls excluding all cancers)",
                "Type 1 diabetes, wide definition, subgroup 2",
                "Disorders related to short gestation",
                "Dementia, including avohilmo",
                "Dementia",
                "Diabetic neuropathy",
                "Heart failure, not strict",
                "Hernia of abdominal wall, postoperative",
                "Hypertension",
                "Asthma-related pneumonia",
                "Asthma-related infections",
                "Pre-eclampsia or poor fetal growth",
                "Asthma and allergy",
                "Asthma and opportunity respiratory infection",
                "Iridocyclitis",
                "Mixed and other personality disorders",
                "Malignant neoplasm of rectum (controls excluding all cancers)",
                "Suppurative and unspecified otitis media",
                "Pre-eclampsia",
                "Viral pneumonia (unknown virus, not influenza)",
                "Diabetes, several complications",
                "DVT of lower extremities and pulmonary embolism",
                "Seropositive rheumatoid arthritis",
                "Seropositive rheumatoid arthritis, wide",
                "Alzheimer’s disease (undefined) (more control exclusions)",
                "Vascular dementia (other)",
                "Vascular dementia (undefined)",
                "Psoriasis",
                "Frontotemporal dementia, wide, incl. Pick's disease",
                "Tic disorders",
                "Parkinson's disease",
                "Generalized epilepsy, mode (most common among epilepsy diagnosis)",
                "Generalized epilepsy",
                "Epileptic seizures related to drugs",
                "Cluster headache, wide",
                "Sleep apnoea, including avohilmo",
                "Vitreous bleeding (caused by prolif. dmrp when together with H36.03*)",
                "Stroke, excluding SAH",
                "COPD (mode)",
                "Asthma (only as main-diagnosis) (more control exclusions)",
                "Supernumerary teeth EXCLUDE clefts and syndromes , including avohilmo",
                "Hypoplasia of dental enamel , including avohilmo",
                "Hypoplasia of dental enamel, prenatal , including avohilmo",
                "Erosion of teeth, including avohilmo",
                "Mandibular prognathia AND surgery (LeFortI or BSSO or LeFortI AND BSSO) , including avohilmo",
                "Hyperkinetic disorders (more control exclusions)",
                "Recurring oral aphthae, including avohilmo",
                "Oral leukoplakia and related diseases, including avohilmo",
                "Oral leukoplakia, including avohilmo",
                "Oral lichen ruber planus, including avohilmo",
                "Oral lichen ruber planus wide, hypothyreosis or thyroxin",
                "Oral lichen ruber planus, wide definition",
                "Respiratory tuberculosis, not confirmed bacteriologically or histologically",
                "Artrosis, including avohilmo",
                "Cholangitis (primary sclerosing, PSC), with reimbursement 202",
                "Behavioural syndromes associated with physiological disturbances and physical factors",
                "Mood [affective] disorders",
                "Kela reimbursement Agammaglobulinemia (D80-84)",
                "Kela-code for severe mental illness",
                "Kela-code for behavioural disturbances in mental retardation",
                "Asthma/COPD (KELA code 203)",
                "IBD patients in KELA-register",
                "Universal erythroderma, KELA reimbursement",
                "Alcohol use disorder, Swedish definition",
                "Polycystic ovarian syndrome, broad definition",
                "Polycystic ovarian syndrome, broad definition",
                "Cardiomyopathy (excluding other)",
                "Suggestive for eosinophilic asthma",
                "Cholelithiasis, broad definition with cholecystitis",
                "Cirrhosis, broad definition used in the article https://doi.org/10.1101/594523",
                "Autoimmune diseases excluding thyroid diseases",
                "Gout",
                "Atrial fibrillation and flutter with reimbursement",
                "Diabetic retinopathy",
                "Focal epilepsy, mode (most common among epilepsy diagnosis)",
                "Focal epilepsy",
                "Generalized epilepsy, mode (most common among epilepsy diagnosis)",
                "Generalized epilepsy",
                "All-cause Heart Failure",
                "Heart failure and antihypertensive medication",
                "Heart failure and coronary heart disease",
                "Heart failure and hypertrophic cardiomyopathy",
                "Interstitial lung disease endpoints",
                "Systemic sclerosis",
                "ILD-related respiratory insufficiency",
                "ILD related to systemic autoimmune disease",
                "IBD patients in KELA-register",
                "KELA_REIMBURSEMENT_202",
                "Migraine, single triptan purchase ok & required. ICD-code if available is included",
                "Endometriosis diagnosis and infertility diagnosis occurring together",
                "Reactive arthropathies, FINNGEN",
                "Medical treatment for female infertility",
                "Osteoporosis with pathological fracture (FG)",
                "Other systemic involvement of connective tissue (FG)",
                "Bleeding",
                "Cardiovascular diseases (excluding rheumatic etc)",
                "Continuous positive airway pressure",
                "Calcific aortic valvular stenosis, operated",
                "SLE (Finngen)",
                "Hard cardiovascular diseases",
                "Asymptomatic human immunodeficiency virus [HIV] infection status",
                "Benign neoplasm: Colon",
                "Parodontitis or/and operation codes",
                "Coxarthrosis,"
)

finn <- subset(finn, !(finn$name %in% exclusions))
finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$name %in% exclusions, "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)


finn <- subset(finn, !grepl("Juvenile", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Juvenile", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("Childhood", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Childhood", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("childhood", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("childhood", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("adolescence", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("adolescence", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("fetus and newborn", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("fetus and newborn", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("Fetus and newborn", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("Fetus and newborn", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("newborn", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("newborn", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)


finn <- subset(finn, !startsWith(finn$name, "Birth"))
finn_before_exclusions$reason_if_excluded <- ifelse(
  startsWith(finn_before_exclusions$name, "Birth"), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !grepl("perinatal", finn$name))
finn_before_exclusions$reason_if_excluded <- ifelse(
  grepl("perinatal", finn_before_exclusions$name), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

finn <- subset(finn, !startsWith(finn$ICD, "Q") | is.na(finn$ICD))
finn_before_exclusions$reason_if_excluded <- ifelse(
  !(!startsWith(finn_before_exclusions$ICD, "Q") | is.na(finn_before_exclusions$ICD)), "Congenital / juvenile / developmental", finn_before_exclusions$reason_if_excluded
)

exclusions <- c("Localized adiposity",
                "Obesity",
                "Obesity related asthma",
                "Obesity, other/unspecified",
                "Obesity due to excess calories",
                "Extreme obesity with alveolar hypoventilation",
                "Heart failure and bmi 25plus")

finn <- subset(finn, !(finn$name %in% exclusions))

finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$name %in% exclusions, "Superfluous", finn_before_exclusions$reason_if_excluded
)


finn_before_exclusions$ratio <- finn_before_exclusions$num_cases/finn_before_exclusions$num_controls
finn_before_exclusions$N <- finn_before_exclusions$num_controls + finn_before_exclusions$num_cases
finn_before_exclusions$b1 <- log(1.5)
sig <- 0.05

rsq <- 0.067191014705

finn_before_exclusions$power <- pnorm(sqrt(finn_before_exclusions$N*rsq*(finn_before_exclusions$ratio/(1+finn_before_exclusions$ratio))*(1/(1+finn_before_exclusions$ratio)))*finn_before_exclusions$b1-qnorm(1-sig/2))


finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$power <= 0.8 & !is.na(finn_before_exclusions$reason_if_excluded), paste0("(Power below 80% and) ", finn_before_exclusions$reason_if_excluded), finn_before_exclusions$reason_if_excluded
)


finn_before_exclusions$reason_if_excluded <- ifelse(
  finn_before_exclusions$power <= 0.8 & is.na(finn_before_exclusions$reason_if_excluded), "Power below 80%", finn_before_exclusions$reason_if_excluded
)


finn_before_exclusions$reason_if_excluded <- ifelse(
  is.na(finn_before_exclusions$reason_if_excluded), "Included", finn_before_exclusions$reason_if_excluded
)

finn_before_exclusions <- finn_before_exclusions[, c("name", "category",
                                                     "LONGNAME", "ICD",
                                                     "power", "reason_if_excluded",
                                                     "num_cases", "num_controls",
                                                     "phenocode", "CONTROL_EXCLUDE")]

colnames(finn_before_exclusions) <- c("name", "category",
                                      "long name", "ICD 10 code",
                                      "power estimate", "reason for exclusion (if excluded)",
                                      "cases", "controls",
                                      "phenocode", "phenocodes excluded from controls")


colnames(finn_before_exclusions)

#write.csv(finn_before_exclusions, 'supplementary_table_S1.csv')

finn$ratio <- finn$num_cases/finn$num_controls
finn$N <- finn$num_controls + finn$num_cases
finn$b1 <- log(1.5)
sig <- 0.05

rsq <- 0.067191014705

finn$power <- pnorm(sqrt(finn$N*rsq*(finn$ratio/(1+finn$ratio))*(1/(1+finn$ratio)))*finn$b1-qnorm(1-sig/2))

finn <- subset(finn, finn$power >= 0.8) #[, c("trait", "ncase", "ncontrol", "ratio", "N", #"power")]

#processing FM (MVMR with FFM)

mr <- read.csv("path_to_MR_output/ukb_finngen_r8_mr_nooverlap_01.csv", row.names=1)

contmix <- read_excel("path_to_MR_output/mr_contmix_all_finngen_r8_mr_01.xlsx")
contmix <- contmix %>%
  dplyr::select(-c("Psi", "CIRange", "CIMin", "CIMax", "CIStep", "ValidSNPs", "Valid", "Alpha"))

colnames(contmix) <- c("exposure", "outcome", "b", "lwr", "upr", "pval", "nsnp")
contmix$id.outcome <- contmix$outcome
contmix$id.exposure <- contmix$exposure
contmix$method <- "Contamination mixture"
contmix$se <- (contmix$upr - contmix$b)/1.96
#contmix$se <-NA
contmix$direction <- NA
contmix$direction_p <- NA

length(unique(mr$id.outcome))

mvmr <- read.csv("path_to_MR_output/ukb_finngen_r8_mvmr_01.csv", row.names=1)

mvmr <- subset(mvmr, !startsWith(mvmr$exposure, "Whole body fat-free mass"))
mvmr$method <- "MVMR"

mr <- rbind(mr, mvmr)

length(unique(mr$id.outcome))

mr <- subset(mr, mr$id.outcome %in% unique(finn$name))

run <- unique(mr$id.outcome)

shouldberun <- unique(finn$name)
run <- unlist(run)

notrunyet <- setdiff(shouldberun, run)

mr$outcome <- str_extract(mr$id.outcome, "[^\\|]+")
mr$exposure <- str_extract(mr$exposure, "[^\\|]+")

contmix$outcome <- str_extract(contmix$id.outcome, "[^\\|]+")
contmix$exposure <- str_extract(contmix$exposure, "[^\\|]+")

mr$or <- exp(mr$b)
mr$or <- mr$or^(9.482533808131373)

contmix$or <- exp(contmix$b)
contmix$or <- contmix$or^(9.482533808131373)

mr$lwr <- exp(mr$b-(1.96*mr$se))
mr$lwr <- mr$lwr^(9.482533808131373)

contmix$lwr <- exp(contmix$lwr)
contmix$lwr <- contmix$lwr^(9.482533808131373)

mr$upr <- exp(mr$b+(1.96*mr$se))
mr$upr <- mr$upr^(9.482533808131373)

contmix$upr <- exp(contmix$upr)
contmix$upr <- contmix$upr^(9.482533808131373)

mr <- subset(mr, mr$method!="Weighted mode")

colnames(mr)
colnames(contmix)

mr <- rbind(mr, contmix)

mr$method <- factor(mr$method, levels = c("Inverse variance weighted", "MVMR", "Weighted median", "Contamination mixture", "MR Egger intercept", "MR Egger"))

mr <- merge(mr, finn, by.x="id.outcome", by.y="name", all.x = TRUE)
#mr$ICD <- ifelse(!is.na(mr$HD_ICD_10), mr$HD_ICD_10, ifelse(!is.na(mr$COD_ICD_10), mr$COD_ICD_10, mr$OUTPAT_ICD))

mr$b <- mr$b*(9.482533808131373)
mr$se <- mr$se*(9.482533808131373)


fm <- mr



fm$outcome <- ifelse(fm$outcome=="STROKE",
                     "Stroke", fm$outcome)


fm$outcome <- gsub("\\(controls excluding all cancers\\)", "", fm$outcome)



fm$category <- ifelse(startsWith(fm$outcome, "Ischaemic heart disease"),
                      "Circulatory system", fm$category)
fm$category <- ifelse(startsWith(fm$outcome, "Alzheimer"),
                      "Nervous system", fm$category)

fm$category <- ifelse(grepl("dementia", fm$outcome),
                      "Nervous system", fm$category)
fm$category <- ifelse(grepl("stroke", fm$outcome),
                      "Circulatory system", fm$category)
fm$category <- ifelse(grepl("Stroke", fm$outcome),
                      "Circulatory system", fm$category)
fm$category <- ifelse(grepl("STROKE", fm$outcome),
                      "Circulatory system", fm$category)
fm$category <- ifelse(grepl("Depression", fm$outcome),
                      "Mental and behavioural disorders", fm$category)
fm$category <- ifelse(grepl("Schizophrenia", fm$outcome),
                      "Mental and behavioural disorders", fm$category)



fm$category <- ifelse(grepl("Asthma", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("COPD", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("All influenza", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Allergic asthma", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("Allergic rhinitis", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Cardiac", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Cardiovascular diseases", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Peripheral angiopathy", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("cardiovascular diseases", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Heart failure and coronary heart disease", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Diabet", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)

fm$category <- ifelse(grepl("Gestational diabetes", fm$outcome),
                      "Pregnancy, childbirth and the puerperium", fm$category)

fm$category <- ifelse(grepl("Type1", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)
fm$category <- ifelse(grepl("Type 1", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)
fm$category <- ifelse(grepl("Type2", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)
fm$category <- ifelse(grepl("Type 2", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)

fm$category <- ifelse(grepl("limb, back, neck, head abdominally", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)
fm$category <- ifelse(grepl("gonarthrosis", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)
fm$category <- ifelse(grepl("Postmenopausal osteoporosis with pathological fracture", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)


fm$category <- ifelse(grepl("arthritis", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Sleep disorders", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Smoking", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Anxiety disorders", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Crohn's disease", fm$outcome),
                      "Digestive system", fm$category)

fm$category <- ifelse(grepl("Ulcerative enterocolitis", fm$outcome),
                      "Digestive system", fm$category)
fm$category <- ifelse(grepl("Ulcerative ileocolitis", fm$outcome),
                      "Digestive system", fm$category)
fm$category <- ifelse(grepl("Ulcerative proctitis", fm$outcome),
                      "Digestive system", fm$category)
fm$category <- ifelse(grepl("Ulcerative rectosigmoiditis", fm$outcome),
                      "Digestive system", fm$category)


fm$category <- ifelse(grepl("Vasomotor rhinitis \\(mode\\)", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Viral pneumonia \\(unknown virus, not influenza\\)", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("Vocal cord dysfunction", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("Essential (haemorrhagic) thrombocythaemia", fm$outcome),
                      "Neoplasms", fm$category)


fm$category <- ifelse(grepl("Polycythaemia vera", fm$outcome),
                      "Neoplasms", fm$category)


fm$category <- ifelse(grepl("Gastro-oesophageal reflux disease", fm$outcome),
                      "Digestive system", fm$category)

fm$category <- ifelse(grepl("Mucosal proctocolitis", fm$outcome),
                      "Digestive system", fm$category)



fm$category <- ifelse(grepl("STROKE", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Type 2 diabetes, definitions combined", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)

fm$category <- ifelse(grepl("Thyroiditis", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)

fm$category <- ifelse(grepl("Thyroiditis", fm$outcome),
                      "Endocrine, nutritional and metabolic diseases", fm$category)

fm$category <- ifelse(grepl("Thyrotoxicosis", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Bronchitis", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("Any mental disorder", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Cardiomyopathy", fm$outcome),
                      "Circulatory system", fm$category)

fm$category <- ifelse(grepl("Primary coxarthrosis", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Primary coxarthrosis", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("SLE, strict definition", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("TMD muscular pain linked with fibromyalgia", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)


fm$category <- ifelse(grepl("TMD related pain", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Transient global amnesia", fm$outcome),
                      "Nervous system", fm$category)

fm$category <- ifelse(grepl("Autoimmune diseases", fm$outcome),
                      "Diseases of blood and blood-forming organs and immune disorders", fm$category)

fm$category <- ifelse(grepl("Myeloproliferative diseases \\(CML excluded\\)", fm$outcome),
                      "Diseases of blood and blood-forming organs and immune disorders", fm$category)

fm$category <- ifelse(grepl("Neovascular glaucoma", fm$outcome),
                      "Eye and adnexa", fm$category)

fm$category <- ifelse(grepl("Proliferative diabetic retinopathy", fm$outcome),
                      "Eye and adnexa", fm$category)

fm$category <- ifelse(grepl("severe traumatic brain injury, does not include concussion", fm$outcome), "Injury, poisoning and certain other consequences of external causes", fm$category)


fm$category <- ifelse(grepl("Autoimmune diseases", fm$outcome),
                      "Diseases of blood and blood-forming organs and immune disorders", fm$category)

fm$category <- ifelse(grepl("Essential", fm$outcome) & grepl("thrombocythaemia", fm$outcome),
                      "Neoplasms", fm$category)

fm$category <- ifelse(grepl("Memory loss", fm$outcome),
                      "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", fm$category)

fm$category <- ifelse(grepl("Falls/tendency to fall", fm$outcome),
                      "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", fm$category)

fm$category <- ifelse(grepl("Gout, strict definition", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Spondylopathies", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Idiopathic gout", fm$outcome),
                      "Musculoskeletal system and connective tissue", fm$category)

fm$category <- ifelse(grepl("Idiopathic pulmonary fibrosis", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Interstitial lung disease", fm$outcome),
                      "Respiratory system", fm$category)

fm$category <- ifelse(grepl("Pollen allergy", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Non-allergic asthma \\(mode\\) \\(more control exclusions\\)", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Nonalcoholic fatty liver disease", fm$outcome),
                      "Digestive system", fm$category)

fm$category <- ifelse(grepl("KRA_PSY_EATING \\(more control exclusions\\)", fm$outcome),
                      "Mental and behavioural disorders", fm$category)


fm$category <- ifelse(grepl("Nonorganic sleeping disorders \\(more control exclusions\\)", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Mood disorders \\(more control exclusions\\)", fm$outcome),
                      "Mental and behavioural disorders", fm$category)

fm$category <- ifelse(grepl("Pneumococcal septicemia", fm$outcome),
                      "Infectious and parasitic diseases", fm$category)


fm$category <- ifelse(grepl("Substance abuse \\(more control exclusions\\)", fm$outcome),
                      "Mental and behavioural disorders", fm$category)
fm$category <- ifelse(grepl("Personality disorders \\(more control exclusions\\)", fm$outcome),
                      "Mental and behavioural disorders", fm$category)
fm$category <- ifelse(grepl("Alcohol abuse", fm$outcome),
                      "Mental and behavioural disorders", fm$category)
fm$category <- ifelse(grepl("Alcohol related diseases and deaths", fm$outcome),
                      "Mental and behavioural disorders", fm$category)
fm$category <- ifelse(grepl("Covid-19", fm$outcome),
                      "Respiratory system", fm$category)


fm$category <- ifelse(grepl("Suicide or other Intentional self-harm", fm$outcome),
                      "External causes of morbidity and mortality", fm$category)


fm$category <- ifelse(grepl("Meningitis", fm$outcome),
                      "Nervous system", fm$category)

fm$category <- ifelse(grepl("Parkinson", fm$outcome),
                      "Nervous system", fm$category)

fm$outcome <- gsub(" \\(controls excluding all cancers\\)", "", fm$outcome)

fm$outcome <- gsub("Type 2 diabetes, wide definition", "Type 2 diabetes", fm$outcome)
fm$outcome <- gsub("Asthma (more control exclusions)", "Asthma", fm$outcome)

fm$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", fm$outcome)
fm$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", fm$outcome)
fm$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", fm$outcome)
fm$outcome <-ifelse(startsWith(fm$outcome, "Heart failure"), "Heart failure", fm$outcome)
fm$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", fm$outcome)
fm$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", fm$outcome)
fm$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", fm$outcome)
fm$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", fm$outcome)

fm$outcome <- str_replace_all(fm$outcome, "\\(controls excluding all cancers\\)", "")

fm$outcome <- str_replace_all(fm$outcome, "\\(more control exclusions\\)", "")

fm$outcome <- str_replace_all(fm$outcome, "\\(FINNGEN\\)", "")

fm$outcome <- str_replace_all(fm$outcome, ", only avohilmo", "")

fm$outcome <- str_replace_all(fm$outcome, ", wide definition", "")


unclass <- subset(fm, is.na(fm$category))


#processing FFM (mvmr with FM)

mr <- read.csv("path_to_MR_output/ukb_ffm_finngen_r8_mr_nooverlap_01.csv", row.names=1)

contmix <- read_excel("path_to_MR_output/mr_contmix_all_ffm_finngen_r8_mr_01.xlsx")
contmix <- contmix %>%
  dplyr::select(-c("Psi", "CIRange", "CIMin", "CIMax", "CIStep", "ValidSNPs", "Valid", "Alpha"))

colnames(contmix) <- c("exposure", "outcome", "b", "lwr", "upr", "pval", "nsnp")
contmix$id.outcome <- contmix$outcome
contmix$id.exposure <- contmix$exposure
contmix$method <- "Contamination mixture"
contmix$se <- (contmix$upr - contmix$b)/1.96
#contmix$se <-NA
contmix$direction <- NA
contmix$direction_p <- NA

length(unique(mr$id.outcome))

mvmr <- read.csv("path_to_MR_output/ukb_finngen_r8_mvmr_01.csv", row.names=1)

mvmr <- subset(mvmr, !startsWith(mvmr$exposure, "Whole body fat mass"))
mvmr$method <- "MVMR"

mr <- rbind(mr, mvmr)

mr <- subset(mr, mr$id.outcome %in% unique(finn$name))

length(unique(mr$id.outcome))


mr$outcome <- str_extract(mr$id.outcome, "[^\\|]+")
mr$exposure <- str_extract(mr$exposure, "[^\\|]+")

contmix$outcome <- str_extract(contmix$id.outcome, "[^\\|]+")
contmix$exposure <- str_extract(contmix$exposure, "[^\\|]+")

mr$or <- exp(mr$b)
mr$or <- mr$or^(11.524726430457314)

contmix$or <- exp(contmix$b)
contmix$or <- contmix$or^(11.524726430457314)

mr$lwr <- exp(mr$b-(1.96*mr$se))
mr$lwr <- mr$lwr^(11.524726430457314)

contmix$lwr <- exp(contmix$lwr)
contmix$lwr <- contmix$lwr^(11.524726430457314)

mr$upr <- exp(mr$b+(1.96*mr$se))
mr$upr <- mr$upr^(11.524726430457314)

contmix$upr <- exp(contmix$upr)
contmix$upr <- contmix$upr^(11.524726430457314)

mr <- subset(mr, mr$method!="Weighted mode")

mr <- rbind(mr, contmix)

mr$method <- factor(mr$method, levels = c("Inverse variance weighted", "MVMR", "Weighted median", "Contamination mixture", "MR Egger intercept", "MR Egger"))

mr <- merge(mr, finn, by.x="id.outcome", by.y="name")
#mr$ICD <- ifelse(!is.na(mr$HD_ICD_10), mr$HD_ICD_10, ifelse(!is.na(mr$COD_ICD_10), mr$COD_ICD_10, mr$OUTPAT_ICD))

mr$b <- mr$b*(11.524726430457314)
mr$se <- mr$se*(11.524726430457314)

ffm <- mr


ffm$outcome <- ifelse(ffm$outcome=="STROKE",
                      "Stroke", ffm$outcome)

ffm$outcome <- gsub("\\(controls excluding all cancers\\)", "", ffm$outcome)

ffm$category <- ifelse(startsWith(ffm$outcome, "Ischaemic heart disease"),
                       "Circulatory system", ffm$category)
ffm$category <- ifelse(startsWith(ffm$outcome, "Alzheimer"),
                       "Nervous system", ffm$category)
ffm$category <- ifelse(grepl("dementia", ffm$outcome),
                       "Nervous system", ffm$category)
ffm$category <- ifelse(grepl("stroke", ffm$outcome),
                       "Circulatory system", ffm$category)
ffm$category <- ifelse(grepl("Stroke", ffm$outcome),
                       "Circulatory system", ffm$category)
ffm$category <- ifelse(grepl("STROKE", ffm$outcome),
                       "Circulatory system", ffm$category)
ffm$category <- ifelse(grepl("Depression", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)
ffm$category <- ifelse(grepl("Schizophrenia", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)



ffm$category <- ifelse(grepl("Asthma", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("COPD", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("All influenza", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Allergic asthma", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("Allergic rhinitis", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Cardiac", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Cardiovascular diseases", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Peripheral angiopathy", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("cardiovascular diseases", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Heart failure and coronary heart disease", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Diabet", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)

ffm$category <- ifelse(grepl("Gestational diabetes", ffm$outcome),
                       "Pregnancy, childbirth and the puerperium", ffm$category)

ffm$category <- ifelse(grepl("Type1", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)
ffm$category <- ifelse(grepl("Type 1", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)
ffm$category <- ifelse(grepl("Type2", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)
ffm$category <- ifelse(grepl("Type 2", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)

ffm$category <- ifelse(grepl("limb, back, neck, head abdominally", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)
ffm$category <- ifelse(grepl("gonarthrosis", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)
ffm$category <- ifelse(grepl("Postmenopausal osteoporosis with pathological fracture", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)


ffm$category <- ifelse(grepl("arthritis", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Sleep disorders", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Smoking", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Anxiety disorders", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Crohn's disease", ffm$outcome),
                       "Digestive system", ffm$category)

ffm$category <- ifelse(grepl("Ulcerative enterocolitis", ffm$outcome),
                       "Digestive system", ffm$category)
ffm$category <- ifelse(grepl("Ulcerative ileocolitis", ffm$outcome),
                       "Digestive system", ffm$category)
ffm$category <- ifelse(grepl("Ulcerative proctitis", ffm$outcome),
                       "Digestive system", ffm$category)
ffm$category <- ifelse(grepl("Ulcerative rectosigmoiditis", ffm$outcome),
                       "Digestive system", ffm$category)


ffm$category <- ifelse(grepl("Vasomotor rhinitis \\(mode\\)", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Viral pneumonia \\(unknown virus, not influenza\\)", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("Vocal cord dysfunction", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("Essential (haemorrhagic) thrombocythaemia", ffm$outcome),
                       "Neoplasms", ffm$category)


ffm$category <- ifelse(grepl("Polycythaemia vera", ffm$outcome),
                       "Neoplasms", ffm$category)


ffm$category <- ifelse(grepl("Gastro-oesophageal reflux disease", ffm$outcome),
                       "Digestive system", ffm$category)

ffm$category <- ifelse(grepl("Mucosal proctocolitis", ffm$outcome),
                       "Digestive system", ffm$category)



ffm$category <- ifelse(grepl("STROKE", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Type 2 diabetes, definitions combined", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)

ffm$category <- ifelse(grepl("Thyroiditis", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)

ffm$category <- ifelse(grepl("Thyroiditis", ffm$outcome),
                       "Endocrine, nutritional and metabolic diseases", ffm$category)

ffm$category <- ifelse(grepl("Thyrotoxicosis", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Bronchitis", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("Any mental disorder", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Cardiomyopathy", ffm$outcome),
                       "Circulatory system", ffm$category)

ffm$category <- ifelse(grepl("Primary coxarthrosis", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Primary coxarthrosis", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("SLE, strict definition", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("TMD muscular pain linked with fibromyalgia", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)


ffm$category <- ifelse(grepl("TMD related pain", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Transient global amnesia", ffm$outcome),
                       "Nervous system", ffm$category)

ffm$category <- ifelse(grepl("Autoimmune diseases", ffm$outcome),
                       "Diseases of blood and blood-forming organs and immune disorders", ffm$category)

ffm$category <- ifelse(grepl("Myeloproliferative diseases \\(CML excluded\\)", ffm$outcome),
                       "Diseases of blood and blood-forming organs and immune disorders", ffm$category)

ffm$category <- ifelse(grepl("Neovascular glaucoma", ffm$outcome),
                       "Eye and adnexa", ffm$category)

ffm$category <- ifelse(grepl("Proliferative diabetic retinopathy", ffm$outcome),
                       "Eye and adnexa", ffm$category)

ffm$category <- ifelse(grepl("severe traumatic brain injury, does not include concussion", ffm$outcome), "Injury, poisoning and certain other consequences of external causes", ffm$category)


ffm$category <- ifelse(grepl("Autoimmune diseases", ffm$outcome),
                       "Diseases of blood and blood-forming organs and immune disorders", ffm$category)

ffm$category <- ifelse(grepl("Essential", ffm$outcome) & grepl("thrombocythaemia", ffm$outcome),
                       "Neoplasms", ffm$category)

ffm$category <- ifelse(grepl("Memory loss", ffm$outcome),
                       "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", ffm$category)

ffm$category <- ifelse(grepl("Falls/tendency to fall", ffm$outcome),
                       "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", ffm$category)

ffm$category <- ifelse(grepl("Gout, strict definition", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Spondylopathies", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Idiopathic gout", ffm$outcome),
                       "Musculoskeletal system and connective tissue", ffm$category)

ffm$category <- ifelse(grepl("Idiopathic pulmonary fibrosis", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Interstitial lung disease", ffm$outcome),
                       "Respiratory system", ffm$category)

ffm$category <- ifelse(grepl("Pollen allergy", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Non-allergic asthma \\(mode\\) \\(more control exclusions\\)", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Nonalcoholic fatty liver disease", ffm$outcome),
                       "Digestive system", ffm$category)

ffm$category <- ifelse(grepl("KRA_PSY_EATING \\(more control exclusions\\)", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)


ffm$category <- ifelse(grepl("Nonorganic sleeping disorders \\(more control exclusions\\)", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Mood disorders \\(more control exclusions\\)", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)

ffm$category <- ifelse(grepl("Pneumococcal septicemia", ffm$outcome),
                       "Infectious and parasitic diseases", ffm$category)


ffm$category <- ifelse(grepl("Substance abuse \\(more control exclusions\\)", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)
ffm$category <- ifelse(grepl("Personality disorders \\(more control exclusions\\)", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)
ffm$category <- ifelse(grepl("Alcohol abuse", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)
ffm$category <- ifelse(grepl("Alcohol related diseases and deaths", ffm$outcome),
                       "Mental and behavioural disorders", ffm$category)
ffm$category <- ifelse(grepl("Covid-19", ffm$outcome),
                       "Respiratory system", ffm$category)


ffm$category <- ifelse(grepl("Suicide or other Intentional self-harm", ffm$outcome),
                       "External causes of morbidity and mortality", ffm$category)


ffm$category <- ifelse(grepl("Meningitis", ffm$outcome),
                       "Nervous system", ffm$category)

ffm$category <- ifelse(grepl("Parkinson", ffm$outcome),
                       "Nervous system", ffm$category)

unclass <- subset(ffm, is.na(ffm$category))


ffm$outcome <- gsub(" \\(controls excluding all cancers\\)", "", ffm$outcome)

ffm$outcome <- gsub("Type 2 diabetes, wide definition", "Type 2 diabetes", ffm$outcome)
ffm$outcome <- gsub("Asthma (more control exclusions)", "Asthma", ffm$outcome)

ffm$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", ffm$outcome)
ffm$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", ffm$outcome)
ffm$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", ffm$outcome)
ffm$outcome <-ifelse(startsWith(ffm$outcome, "Heart failure"), "Heart failure", ffm$outcome)
ffm$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", ffm$outcome)
ffm$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", ffm$outcome)
ffm$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", ffm$outcome)
ffm$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", ffm$outcome)


ffm$outcome <- str_replace_all(ffm$outcome, "\\(controls excluding all cancers\\)", "")

ffm$outcome <- str_replace_all(ffm$outcome, "\\(more control exclusions\\)", "")

ffm$outcome <- str_replace_all(ffm$outcome, "\\(FINNGEN\\)", "")

ffm$outcome <- str_replace_all(ffm$outcome, ", only avohilmo", "")

ffm$outcome <- str_replace_all(ffm$outcome, ", wide definition", "")

mr <- rbind(fm, ffm)


#interpretation begins

mr <- mr %>%
  group_by(method) %>%
  dplyr::mutate(fdr = p.adjust(pval, method = "fdr"))

mr <- mr %>%
  group_by(method) %>%
  dplyr::mutate(fdr_direction = p.adjust(direction_p, method = "fdr"))

unique(mr$exposure)

mr_fm <- subset(mr, mr$exposure=="Whole body fat mass")
mr_ffm <- subset(mr, mr$exposure=="Whole body fat-free mass")

#FM interpretation
mr <- mr_fm

pleiotropic <- subset(mr, mr$method=="MR Egger intercept" & pval < 0.05)

pleiotropic <- unique(pleiotropic$outcome)

mr <- subset(mr, (mr$outcome %in% pleiotropic) | !(mr$method=="MR Egger"))
mr <- subset(mr, mr$method!="MR Egger intercept")


mr$positive <- ifelse(mr$or > 1, 1, 0)
mr$significant <- ifelse(mr$fdr < 0.05, 1, 0)

mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(positive_sum = sum(positive))

mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(significant_sum = sum(significant))


mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(num_methods = n())

mr$positive_ratio <- mr$positive_sum/mr$num_methods
mr$significant_ratio <- mr$significant_sum/mr$num_methods

mr$p_significant <- ifelse(mr$pval <0.05, 1, 0)
mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(p_significant_sum = sum(p_significant))

mr$p_significant_ratio <- mr$p_significant_sum/mr$num_methods

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(ivw_sig = ifelse(any(method == "Inverse variance weighted" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(egger_sig = ifelse(any(method == "MR Egger" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(median_sig = ifelse(any(method == "Weighted median" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(mode_sig = ifelse(any(method == "Contamination mixture" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(mvmr_sig = ifelse(any(method == "MVMR" & significant == 1), 1, 0))


mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(direction_correct = ifelse(any(method == "Inverse variance weighted" & direction == TRUE), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(direction_correct = ifelse(any(method == "MVMR" & direction == TRUE), 1, 0))

mr$narrow_zero <- ifelse(((mr$or >= 0.94) &
                            (mr$or <= 1.06) &
                            (mr$upr <= 1.16) &
                            (mr$lwr >= 0.84) &
                            (mr$pval >= 0.05) &
                            (mr$method == "Inverse variance weighted") &
                            (mr$significant_ratio == 0)), 1, 0)


zeros <- subset(mr, mr$narrow_zero==1)
zeros <- subset(zeros, zeros$method=="Inverse variance weighted")
zeros <- merge(zeros, finn, by.x="id.outcome", by.y="name")
unique(zeros$category)

mr$interesting <- ifelse(
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$ivw_sig>0) & (mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)

mr$interesting <- ifelse(
  #(mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  #(mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1 ) & (mr$direction_correct==1)
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1) & (mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)

mr$probable <- ifelse(
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & !(mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)



#robust <- subset(mr, interesting==1 & method=="MVMR")
robust <- subset(mr, interesting==1 & method=="Inverse variance weighted")

#robust_prob <- subset(mr, probable==1 & method=="MVMR")
robust_prob <- subset(mr, probable==1 & method=="Inverse variance weighted")

mr_fm <- mr


#FFM interpretation

mr <- mr_ffm

pleiotropic <- subset(mr, mr$method=="MR Egger intercept" & pval < 0.05)

pleiotropic <- unique(pleiotropic$outcome)

mr <- subset(mr, (mr$outcome %in% pleiotropic) | !(mr$method=="MR Egger"))
mr <- subset(mr, mr$method!="MR Egger intercept")

mr$positive <- ifelse(mr$or > 1, 1, 0)
mr$significant <- ifelse(mr$fdr < 0.05, 1, 0)

mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(positive_sum = sum(positive))

mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(significant_sum = sum(significant))

mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(num_methods = n())

mr$positive_ratio <- mr$positive_sum/mr$num_methods
mr$significant_ratio <- mr$significant_sum/mr$num_methods

mr$p_significant <- ifelse(mr$pval <0.05, 1, 0)
mr <- mr %>% group_by(outcome) %>% 
  dplyr::mutate(p_significant_sum = sum(p_significant))

mr$p_significant_ratio <- mr$p_significant_sum/mr$num_methods

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(ivw_sig = ifelse(any(method == "Inverse variance weighted" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(egger_sig = ifelse(any(method == "MR Egger" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(median_sig = ifelse(any(method == "Weighted median" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(mode_sig = ifelse(any(method == "Contamination mixture" & significant == 1), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(mvmr_sig = ifelse(any(method == "MVMR" & significant == 1), 1, 0))


mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(direction_correct = ifelse(any(method == "Inverse variance weighted" & direction == TRUE), 1, 0))

mr <- mr %>% group_by(outcome) %>%
  dplyr::mutate(direction_correct = ifelse(any(method == "MVMR" & direction == TRUE), 1, 0))

mr$narrow_zero <- ifelse(((mr$or >= 0.94) &
                            (mr$or <= 1.06) &
                            (mr$upr <= 1.16) &
                            (mr$lwr >= 0.84) &
                            (mr$pval >= 0.05) &
                            (mr$method == "Inverse variance weighted") &
                            (mr$significant_ratio == 0)), 1, 0)

nrow(subset(mr, mr$narrow_zero==1))


zeros_ffm <- subset(mr, mr$narrow_zero==1)
zeros_ffm <- subset(zeros_ffm, zeros_ffm$method=="Inverse variance weighted")
zeros_ffm <- merge(zeros_ffm, finn, by.x="id.outcome", by.y="name")
unique(zeros_ffm$category)

mr$interesting <- ifelse(
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$ivw_sig>0) & (mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)

mr$interesting <- ifelse(
  #(mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  #(mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1 ) & (mr$direction_correct==1)
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & (mr$median_sig == 1) & (mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)

mr$probable <- ifelse(
  (mr$positive_ratio==1 | mr$positive_ratio==0) & (mr$mvmr_sig>0) & (mr$ivw_sig>0) & !(mr$median_sig == 1 | mr$mode_sig == 1) & (mr$direction_correct==1)
  , ifelse(
    mr$outcome %in% pleiotropic, ifelse(
      mr$egger_sig==1, 1, 0), 1), 0)

mr_ffm <- mr


robust_ffm <- subset(mr, interesting==1 & method=="MVMR")
#robust_ffm <- subset(mr, interesting==1 & method=="Inverse variance weighted")

robust_ffm_prob <- subset(mr, probable==1 & method=="MVMR")
#robust_ffm_prob <- subset(mr, probable==1 & method=="Inverse variance weighted")


robust %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))

robust_prob %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))


robust_ffm_pos <- subset(robust_ffm, robust_ffm$or > 1)
robust_ffm_neg <- subset(robust_ffm, robust_ffm$or < 1)

robust_ffm %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))

robust_ffm_pos %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))

robust_ffm_neg %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))

robust_ffm_prob %>%
  group_by(category) %>%
  summarise(num_unique_outcomes = n_distinct(id.outcome))


heterogeneity_fm <- read.csv("path_to_MR_output/heterogeneity_fm_finngen_r8_mr_nooverlap_01.csv")
heterogeneity_fm <- heterogeneity_fm[, c("id.outcome", "method", "Q", "Q_df", "Q_pval")]
mr_fm <- merge(mr_fm, heterogeneity_fm, by=c("id.outcome", "method"), all.x=T)
mr_fm <- mr_fm[, c("exposure", "outcome", "method", "ICD", "category", "nsnp",
                   "or", "lwr", "upr", "fdr", "pval", "direction",
                   "direction_p", "fdr_direction", "num_cases",
                   "num_controls", "power", "Q", "Q_df", "interesting")]


colnames(mr_fm)

heterogeneity_ffm <- read.csv("path_to_MR_output/heterogeneity_ffm_finngen_r8_mr_nooverlap_01.csv")
heterogeneity_ffm <- heterogeneity_ffm[, c("id.outcome", "method", "Q", "Q_df", "Q_pval")]
mr_ffm <- merge(mr_ffm, heterogeneity_ffm, by=c("id.outcome", "method"), all.x=T)
mr_ffm <- mr_ffm[, c("exposure", "outcome", "method", "ICD", "category", "nsnp",
                     "or", "lwr", "upr", "fdr", "pval", "direction",
                     "direction_p", "fdr_direction", "num_cases",
                     "num_controls", "power", "Q", "Q_df", "interesting")]

mr <- rbind(mr_fm, mr_ffm)

mr$interesting <- ifelse(mr$interesting==1, "YES", "NO")

colnames(mr) <- c("exposure", "outcome", "method", "ICD 10 code", "outcome category",
                  "number of SNPs", "odds ratio", "95% CI lower", "95% CI upper",
                  "FDR-adjusted p-value", "p-value", "correct MR-Steiger direction",
                  "MR-Stegier p-value", "MR-Steiger FDR-adjusted p-value", "number of cases",
                  "number of controls", "power", "Q", "Q_df", "robust and independent association")


write.csv(mr, 'supplementary_table_S7.csv')
