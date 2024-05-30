library(dplyr)
#install.packages("dplyr")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
##install.packages("dplyr")
library(ggplot2)
#install.packages("dplyr")
library(stringr)
#install.packages("R.utils")
library(R.utils)
#install.packages("ckbplotr")
library(ckbplotr)
#remotes::install_github("mrcieu/ieugwasr")
library(ieugwasr)
#install.packages("qqman")
library(qqman)
#install.packages("ggtext")
library(ggtext)
#install.packages("curl")
library(curl)


fat_mass_regenie <- read.delim("path_to_merged_regenie_output/assoc.fat_mass_37.regenie.merged.txt")


fat_mass_regenie <- 
  fat_mass_regenie %>% mutate(P = (10^(-LOG10P)))

nrow(fat_mass_regenie)

fat_mass_regenie <- 
  fat_mass_regenie %>%
  filter(TEST == "ADD") %>%
  tidyr::drop_na(LOG10P)

nrow(fat_mass_regenie)

fat_mass_regenie$pval.exposure <- fat_mass_regenie$P
fat_mass_regenie$id.exposure <- "Whole body fat mass"
fat_mass_regenie$chr_name <- fat_mass_regenie$CHROM
fat_mass_regenie$chrom_start <- fat_mass_regenie$GENPOS
fat_mass_regenie$SNP <- fat_mass_regenie$ID

fat_free_mass_regenie <- read.delim("path_to_merged_regenie_output/assoc.fat_free_mass_37.regenie.merged.txt")


fat_free_mass_regenie <- 
  fat_free_mass_regenie %>% mutate(P = (10^(-LOG10P)))

nrow(fat_free_mass_regenie)

fat_free_mass_regenie <- 
  fat_free_mass_regenie %>%
  filter(TEST == "ADD") %>%
  tidyr::drop_na(LOG10P)

nrow(fat_free_mass_regenie)

nrow(subset(fat_free_mass_regenie, fat_free_mass_regenie$P < 5e-08))


fat_free_mass_regenie$pval.exposure <- fat_free_mass_regenie$P
fat_free_mass_regenie$id.exposure <- "Whole body fat-free mass"
fat_free_mass_regenie$chr_name <- fat_free_mass_regenie$CHROM
fat_free_mass_regenie$chrom_start <- fat_free_mass_regenie$GENPOS
fat_free_mass_regenie$SNP <- fat_free_mass_regenie$ID


set.seed(2404)
#fm_all_snps <- extract_instruments("ukb-b-19393", clump=FALSE, p1=0.05, p2=1)

fm_all_snps <- fat_mass_regenie
snpsOfInterest <- unique(fm_instruments_ukb$SNP)


fm_all_snps$chr.exposure <- as.numeric(fm_all_snps$CHROM)
fm_all_snps$pos.exposure <- as.numeric(fm_all_snps$GENPOS)


sig_data <- fm_all_snps %>% 
  subset(pval.exposure < 5e-8)
notsig_data <- fm_all_snps %>% 
  subset(pval.exposure >= 5e-8) %>%
  group_by(chr.exposure) %>% 
  sample_frac(0.2)
fm_all_snps <- bind_rows(sig_data, notsig_data)

don <- fm_all_snps %>% 
  
  # Compute chromosome size
  group_by(chr.exposure) %>% 
  summarise(chr_len=max(pos.exposure)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(fm_all_snps, ., by=c("chr.exposure"="chr.exposure")) %>%
  
  # Add a cumulative position of each SNP
  arrange(chr.exposure, pos.exposure) %>%
  mutate( BPcum=pos.exposure+tot)  %>%
  
  # Add highlight and annotation information
  mutate( is_highlight=ifelse(SNP %in% snpsOfInterest, "yes", "no")) 




axisdf = don %>% group_by(chr.exposure) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
axisdf$chr.exposure[axisdf$chr.exposure == "23"] <- "X"


library(ggthemes)

ggplot(don, aes(x=BPcum, y=-log10(pval.exposure))) +
  
  # Show all points
  geom_point( aes(color=as.factor(chr.exposure)), alpha=0.65, size=1) +
  scale_color_manual(values = rep(c("#276FBF", "#183059"), 22 )) +
  
  geom_hline(yintercept = -log10(5e-8), color = "#E69F00", linetype = "dashed", size=1.25) +  # Adjusted line here
  
  # custom X axis:
  scale_x_continuous(label = axisdf$chr.exposure, breaks= axisdf$center, expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,210)) +     # remove space between plot area and x axis
  
  #geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=1.5) +
  
  # Custom the theme:
  theme_bw(base_size = 12) +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
  )  +
  labs(x = "Chromosome", 
       y = bquote(-log[10](italic(p)))) +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 60, size = 12, vjust = 0.5)
  ) 

ggsave('manhattan_fm.png', width=10, height=4, unit='in', dpi=500)


#FFM
#ffm_all_snps <- extract_instruments("ukb-b-13354", clump=FALSE, p1=0.05, p2=1)

ffm_all_snps <- fat_free_mass_regenie

ffm_all_snps$chr.exposure <- as.numeric(ffm_all_snps$CHROM)
ffm_all_snps$pos.exposure <- as.numeric(ffm_all_snps$GENPOS)

sig_data <- ffm_all_snps %>% 
  subset(pval.exposure < 5e-8)
notsig_data <- ffm_all_snps %>% 
  subset(pval.exposure >= 5e-8) %>%
  group_by(chr.exposure) %>% 
  sample_frac(0.2)

ffm_all_snps <- bind_rows(sig_data, notsig_data)

snpsOfInterest <- unique(ffm_instruments_ukb$SNP)

ffm_all_snps$chr.exposure <- as.numeric(ffm_all_snps$CHROM)
ffm_all_snps$pos.exposure <- as.numeric(ffm_all_snps$GENPOS)


don <- ffm_all_snps %>% 
  
  # Compute chromosome size
  group_by(chr.exposure) %>% 
  summarise(chr_len=max(pos.exposure)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(ffm_all_snps, ., by=c("chr.exposure"="chr.exposure")) %>%
  
  # Add a cumulative position of each SNP
  arrange(chr.exposure, pos.exposure) %>%
  mutate( BPcum=pos.exposure+tot) %>%
  
  # Add highlight and annotation information
  mutate( is_highlight=ifelse(SNP %in% snpsOfInterest, "yes", "no")) 



axisdf = don %>% group_by(chr.exposure) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
axisdf$chr.exposure[axisdf$chr.exposure == "23"] <- "X"


min(don$pval.exposure)

ggplot(don, aes(x=BPcum, y=-log10(pval.exposure))) +
  
  # Show all points
  geom_point( aes(color=as.factor(chr.exposure)), alpha=0.65, size=1) +
  scale_color_manual(values = rep(c("#E69F00", "#F0E442"), 22 )) +
  
  geom_hline(yintercept = -log10(5e-8), color = "#183059", linetype = "dashed", size=1.25) +
  
  # custom X axis:
  scale_x_continuous(label = axisdf$chr.exposure, breaks= axisdf$center, expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +     # remove space between plot area and x axis
  
  #geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=1.5) +
  
  # Custom the theme:
  theme_bw(base_size=12) +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )  +
  labs(x = "Chromosome", 
       y = bquote(-log[10](italic(p)))) +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 60, size = 12, vjust = 0.5)
  ) 

ggsave('manhattan_ffm.png', width=10, height=4, unit='in', dpi=500)
