

R8_release <- read.delim("path_to_finngen_manifest_file/summary_stats_R8_manifest.tsv")

unique(R8_release$category)

nrow(subset(R8_release, is.na(R8_release$category)))

finngen_R8_endpoint_core_noncore_1_0 <- read_excel("FINNGEN_CORE_AND_NONCORE_ENDPOINTS_AND_CONTROLS_DF8_OK (1).xlsx")

sv <- R8_release
finn <- merge(R8_release, finngen_R8_endpoint_core_noncore_1_0, by.x ="phenocode", by.y ="NAME", all.x=T)

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
  (finn_before_exclusions$name=="Vascular diseases of the intestine" & is.na(finn_before_exclusions$LEVEL)), "Duplicate outcome", finn_before_exclusions$reason_if_excluded
)
finn <- subset(finn, !(finn$name=="Vascular diseases of the intestine" & is.na(finn$LEVEL)))

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



finn$ratio <- finn$num_cases/finn$num_controls
finn$N <- finn$num_controls + finn$num_cases
finn$b1 <- log(1.5)
sig <- 0.05

rsq <- 0.067191014705

finn$power <- pnorm(sqrt(finn$N*rsq*(finn$ratio/(1+finn$ratio))*(1/(1+finn$ratio)))*finn$b1-qnorm(1-sig/2))

finn <- subset(finn, finn$power >= 0.8) #[, c("trait", "ncase", "ncontrol", "ratio", "N", #"power")]


