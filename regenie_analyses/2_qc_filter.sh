data_field="ukb22418"
data_file_dir="${project}:/path/to/"

run_plink_qc="plink2 --bfile ${data_field}_all_v2_merged\
 --maf 0.01 --hwe 1e-15 --geno 0.01 \
 --snps-only --chr 1-22 --max-alleles 2 \
 --mind 0.1 --make-bed \
 --out  ${data_field}_allQC_v2_merged"

dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_all_v2_merged.bed"\
   -iin="${data_file_dir}/${data_field}_all_v2_merged.bim" \
   -iin="${data_file_dir}/${data_field}_all_v2_merged.fam"\
   -icmd="${run_plink_qc}" --tag="Step1" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/output_path" --brief --yes
