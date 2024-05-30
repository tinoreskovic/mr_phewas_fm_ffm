library(ckbplotr)

fm_results <- unique(robust$outcome)
ffm_results <- unique(robust_ffm$outcome)

length(unique(robust$category))
length(unique(robust_ffm$category))

overlap_results <- intersect(fm_results, ffm_results)

robust <- subset(mr_fm, mr_fm$method=="MVMR" & (mr_fm$outcome %in% fm_results |
                                                mr_fm$outcome %in% ffm_results))

robust_ffm <- subset(mr_ffm, mr_ffm$method=="MVMR" & (mr_ffm$outcome %in% ffm_results |
                                                      mr_ffm$outcome %in% fm_results))



library(ckbplotr)

#robust$outcome <- ifelse(robust$outcome=="Atrial fibrillation and flutter",
#                       "Atrial fibrillation", robust$outcome)

robust_ffm$outcome <- ifelse(robust_ffm$outcome=="Atrial fibrillation and flutter",
                           "Atrial fibrillation", robust_ffm$outcome)

robust$outcome <- ifelse(robust$outcome=="Hypertension, essential",
                       "Hypertension", robust$outcome)

robust_ffm$outcome <- ifelse(robust_ffm$outcome=="Hypertension, essential",
                           "Hypertension", robust_ffm$outcome)


robust$shape <- 22 #ifelse(robust$method=="Inverse variance weighted", 15, 22)
#robust$fill <- ifelse(robust$interesting, "black", ifelse(robust$probable, "darkgrey", "white"))
robust$fill <- ifelse(robust$interesting, "black", "white")

robust_ffm$shape <- 22 #ifelse(robust$method=="Inverse variance weighted", 15, 22)
#robust_ffm$fill <- ifelse(robust_ffm$interesting, "black", ifelse(robust_ffm$probable, "darkgrey", "white"))
robust_ffm$fill <- ifelse(robust_ffm$interesting, "black", "white")

robust <- subset(robust, robust$method=="MVMR")
robust_ffm <- subset(robust_ffm, robust_ffm$method=="MVMR")


robust$outcome <- str_replace_all(robust$outcome, "\\(.*?\\)", "")
robust_ffm$outcome <- str_replace_all(robust_ffm$outcome, "\\(.*?\\)", "")

robust$outcome <- ifelse(robust$num_cases==151484 & grepl("Pain", robust$outcome), "	
Pain (limb, back, neck, head abdominally)", robust$outcome)

robust_ffm$outcome <- ifelse(robust_ffm$num_cases==151484 & grepl("Pain", robust_ffm$outcome), "	
Pain (limb, back, neck, head abdominally)", robust_ffm$outcome)

#robust %>%
#  group_by(category) %>%
#  summarise(num_unique_outcomes = n_distinct(id.outcome))

robust3 <- robust[, c("outcome", "ICD")]
robust$ICD <- str_replace_all(robust$ICD, "\\|", ",")

causes <- unique(robust$category)

#RESTART HERE

#figure 2

dta <- robust


dta <- subset(dta, grepl("Circulatory", dta$category) |
                grepl("Digestive", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta[order(-dta$or),]
order <- dta$outcome

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


#row_labs <- dta[, 101:103]
row_labs <- dta[, 114:116]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)


row_labs <- row_labs %>% arrange(heading)


ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      showcode = TRUE,
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Circulatory system",                                                    
                               "Digestive system"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FM OR (95% CI)"),
                      xlab = "OR per SD (9.5kg) of FM (95% CI)",
                      #xlab = "OR per 1/2 SD (4.7kg) of FM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases", "Status"),
                      col.left.heading = c("Cases", "Status"),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.5, 4.001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.5, 1, 1.5, 2, 3, 4),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_2a_alt.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)

dta <- robust_ffm

dta <- subset(dta, grepl("Circulatory", dta$category) |
                grepl("Digestive", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta %>%
  mutate(outcome_order = match(outcome, order)) %>%
  arrange(outcome_order) %>%
  dplyr::select(-outcome_order)

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


class(dta$subheading)

row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)

dta$num_cases <- paste0(dta$num_cases, "  ")


row_labs <- row_labs %>% arrange(heading)


library(ckbplotr)

dta$num_cases <- "    "

ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT-FREE MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Circulatory system",                                                    
                               "Digestive system"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FFM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FFM OR (95% CI)"),
                      xlab = "OR per SD (11.5kg) of FFM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases", "Status"),
                      col.left.heading = c("Cases", "Status   "),
                      #col.left.heading = c("Cases   "),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.1, 2.5001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.1, 0.5, 1, 1.5, 2, 2.5),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_2b_alt.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)

# figure 3
table(robust$category)

dta <- robust


dta <- subset(dta, grepl("Endocrine", dta$category) |
                grepl("Diseases of blood", dta$category)|
                grepl("Genitourinary", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta[order(-dta$or),]
order <- dta$outcome

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)


row_labs <- row_labs %>% arrange(heading)

row_labs$heading
ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      showcode = TRUE,
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Diseases of blood and blood-forming organs<br>and immune disorders",
                               "Endocrine, nutritional and metabolic diseases",                                                    
                               "Genitourinary system"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FM OR (95% CI)"),
                      xlab = "OR per SD (9.5kg) of FM (95% CI)",
                      #xlab = "OR per 1/2 SD (4.7kg) of FM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases"),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.5, 4.001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.5, 1, 1.5, 2, 3, 4),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_3a.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)

dta <- robust_ffm

dta <- subset(dta, grepl("Endocrine", dta$category) |
                grepl("Diseases of blood", dta$category)|
                grepl("Genitourinary", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta %>%
  mutate(outcome_order = match(outcome, order)) %>%
  arrange(outcome_order) %>%
  dplyr::select(-outcome_order)

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


class(dta$subheading)

row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)

dta$num_cases <- paste0(dta$num_cases, "  ")


row_labs <- row_labs %>% arrange(heading)


library(ckbplotr)

dta$num_cases <- "    "

ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT-FREE MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Diseases of blood and blood-forming organs<br>and immune disorders",
                               "Endocrine, nutritional and metabolic diseases",                                                    
                               "Genitourinary system"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FFM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FFM OR (95% CI)"),
                      xlab = "OR per SD (11.5kg) of FFM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases   "),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.1, 2.5001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.1, 0.5, 1, 1.5, 2, 2.5),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_3b.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)


# figure 4
table(robust$category)

dta <- robust


dta <- subset(dta, grepl("Infectious", dta$category) |
                grepl("Injury", dta$category)|
                grepl("Mental", dta$category)|
                grepl("Musculoskeletal", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta[order(-dta$or),]
order <- dta$outcome

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)


row_labs <- row_labs %>% arrange(heading)

unique(row_labs$heading)
ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      showcode = TRUE,
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Infectious and parasitic diseases",
                               "Injury, poisoning and certain other consequences<br>of external causes",                                                    
                               "Mental and behavioural disorders",
                               "Musculoskeletal system and connective tissue"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FM OR (95% CI)"),
                      xlab = "OR per SD (9.5kg) of FM (95% CI)",
                      #xlab = "OR per 1/2 SD (4.7kg) of FM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases"),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.5, 4.001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.5, 1, 1.5, 2, 2.5, 3, 4),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_4a.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)

dta <- robust_ffm

dta <- subset(dta, grepl("Infectious", dta$category) |
                grepl("Injury", dta$category)|
                grepl("Mental", dta$category)|
                grepl("Musculoskeletal", dta$category))

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta %>%
  mutate(outcome_order = match(outcome, order)) %>%
  arrange(outcome_order) %>%
  dplyr::select(-outcome_order)

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


class(dta$subheading)

row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)

dta$num_cases <- paste0(dta$num_cases, "  ")


row_labs <- row_labs %>% arrange(heading)


library(ckbplotr)

dta$num_cases <- "    "

ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT-FREE MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Infectious and parasitic diseases",
                               "Injury, poisoning and certain other consequences<br>of external causes",                                                    
                               "Mental and behavioural disorders",
                               "Musculoskeletal system and connective tissue"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FFM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FFM OR (95% CI)"),
                      xlab = "OR per SD (11.5kg) of FFM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases   "),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.3, 2.5001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.3, 1, 1.5, 2, 2.5),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_4b.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)



# figure 5
table(robust$category)

dta <- robust


dta <- subset(dta, grepl("Neoplasms", dta$category) |
                grepl("Nervous", dta$category)|
                grepl("Pregnancy", dta$category)|
                grepl("Respiratory", dta$category) |
                grepl("Skin", dta$category))

dta$outcome <- ifelse(grepl("Bell", dta$outcome), "Bell\\'s palsy", dta$outcome)

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta[order(-dta$or),]
order <- dta$outcome

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)


row_labs <- row_labs %>% arrange(heading)


unique(row_labs$heading)
ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      showcode = TRUE,
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Neoplasms",
                               "Nervous system",                                                    
                               "Pregnancy, childbirth and the puerperium",
                               "Respiratory system",
                               "Skin and subcutaneous tissue"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FM OR (95% CI)"),
                      xlab = "OR per SD (9.5kg) of FM (95% CI)",
                      #xlab = "OR per 1/2 SD (4.7kg) of FM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases"),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.8, 2.5001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.8, 1, 1.5, 2, 2.5),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_5a.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)

dta <- robust_ffm

dta <- subset(dta, grepl("Neoplasms", dta$category) |
                grepl("Nervous", dta$category)|
                grepl("Pregnancy", dta$category)|
                grepl("Respiratory", dta$category) |
                grepl("Skin", dta$category))

dta$outcome <- ifelse(grepl("Bell", dta$outcome), "Bell\\'s palsy", dta$outcome)

dta$outcome <- gsub("Atherosclerosis, excluding cerebral, coronary and PAD", "Atherosclerosis, excluding cerebral, coronary and PAD", dta$outcome)
dta$outcome <- gsub("DVT of lower extremities", "DVT of lower extremities", dta$outcome)
dta$outcome <- gsub("Phlebitis and thrombophlebitis (not including DVT)", "Phlebitis and thrombophlebitis (not including DVT)", dta$outcome)
dta$outcome <-ifelse(startsWith(dta$outcome, "Heart failure"), "Heart failure", dta$outcome)
dta$outcome <- gsub("Secondary hypertension", "Hypertension, secondary", dta$outcome)
dta$outcome <- gsub("Hypertensive Heart Disease", "Hypertensive heart disease", dta$outcome)
dta$outcome <- gsub("Myocardial infarction, strict", "Myocardial infarction", dta$outcome)
dta$outcome <- gsub("Pulmonary embolism", "Pulmonary embolism", dta$outcome)

#dta$outcome <- str_wrap(dta$outcome, 8)

dta <- dta %>%
  mutate(outcome_order = match(outcome, order)) %>%
  arrange(outcome_order) %>%
  dplyr::select(-outcome_order)

dta$key <- paste0(dta$outcome, dta$method)
dta$heading <- dta$category
dta$label <- dta$outcome


class(dta$subheading)

row_labs <- dta[, 101:103]

row_labs$heading <- stringr::str_replace(row_labs$heading, "(.{40}\\S*)\\s", "\\1<br>")
row_labs$label <- stringr::str_replace(row_labs$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$heading <- stringr::str_replace(dta$heading, "(.{40}\\S*)\\s", "\\1<br>")
dta$label <- stringr::str_replace(dta$label, "(.{40}\\S*)\\s", "\\1<br>")
dta$ICD <- stringr::str_replace(dta$ICD, "(.{40}\\S*)\\s", "\\1<br>")

dta$ICD <- gsub("(.{40})", "\\1\n", dta$ICD)
dta$num_cases <- format(dta$num_cases, big.mark = ",")
dta$num_cases <- as.character(dta$num_cases)

dta$label <- gsub("<br>$", "", dta$label)
row_labs$label <- gsub("<br>$", "", row_labs$label)

dta$label <- gsub("\\d+$", "", dta$label)
row_labs$label <- gsub("\\d+$", "", row_labs$label)

dta$num_cases <- paste0(dta$num_cases, "  ")


row_labs <- row_labs %>% arrange(heading)


library(ckbplotr)

dta$num_cases <- "    "

ckbplotr::forest_plot(panels=list(dta),
                      col.key = "key",
                      panel.names = c("FAT-FREE MASS"),
                      row.labels = row_labs,
                      col.estimate = "b",
                      col.stderr = "se",
                      exponentiate = TRUE,
                      #rows = c(#"Circulatory system",
                      #"Endocrine, nutritional<br>and metabolic diseases",
                      #"Genitourinary system",
                      #"Musculoskeletal system<br>and connective tissue",
                      #"Respiratory system"
                      #"Circulatory system"),
                      rows = c("Neoplasms",
                               "Nervous system",                                                    
                               "Pregnancy, childbirth and the puerperium",
                               "Respiratory system",
                               "Skin and subcutaneous tissue"),
                      row.labels.levels = c("heading", "label"),
                      #col.right=c("num_cases"), 
                      #col.right.heading = c("FFM OR (95% CI)", "N. of\ncases"),
                      col.right.heading = c("FFM OR (95% CI)"),
                      xlab = "OR per SD (11.5kg) of FFM (95% CI)",
                      #col.left=c("ICD", "num_cases"),
                      #col.left.heading = c("ICD", "Cases"),
                      col.left=c("num_cases"),
                      col.left.heading = c("Cases   "),
                      pointsize=5, 
                      scalepoints = TRUE,
                      shape="shape", stroke=0.7, ciunder=TRUE, fill="fill",
                      col.left.hjust= 0.5,
                      col.right.hjust= 0.5,
                      plot.margin = margin(8, 8, 8, 8, "mm"),
                      #xlim=c(1, 2.5),
                      #xticks = c(1, 1.5, 2, 2.5),
                      #xlim=c(1, 2.001),
                      xlim=c(0.3, 2.001),
                      #xticks = c(1, 1.5, 2),
                      xticks = c(0.3, 1, 1.5, 2),
                      base_size = 16,
                      mid.space = unit(5, "mm"),
                      col.heading.space = -1) + theme(family="serif")    

ggsave('finngen_fig_5b.pdf',
       width=11, height=12, unit='in', dpi=500)
#width=14, height=26, unit='in', dpi=500)
