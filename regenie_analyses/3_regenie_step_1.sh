data_field="ukb22418"
data_file_dir="${project}:/data/output_path"
txt_file_dir="${project}:/output_path"
 


run_regenie_cmd="regenie --step 1 --out fm_ffm_regenie_37_results \
 --bed ${data_field}_allQC_v2_merged_new_setting \
 --phenoFile fm_ffm_4regenie.phe \
 --covarFile fm_ffm_4regenie.phe \
 --phenoCol fat_mass --phenoCol fat_free_mass \
 --covarCol sex  --covarCol age --covarCol pc{1:10} \
 --catCovarList genotype_measurement_batch \
 --bsize 1000 --loocv --gz --threads 16 --maxCatLevels 120"


dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_allQC_v2_merged.bed"\
   -iin="${data_file_dir}/${data_field}_allQC_v2_merged.bim"\
   -iin="${data_file_dir}/${data_field}_allQC_v2_merged.fam"\
   -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
   -icmd="${run_regenie_cmd}" --tag="Step1" --instance-type "mem1_ssd1_v2_x16" \
   --destination="${project}:/output_path" --brief --yes;


