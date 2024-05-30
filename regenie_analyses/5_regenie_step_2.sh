imp_file_dir="/Bulk/Imputation/UKB imputation from genotype/"
data_field="ukb22828"
data_file_dir="${project}:/output_path"
txt_file_dir="${project}:/output_path"


for chr in {1..22}; do
  run_regenie_cmd="regenie --step 2 --out assoc_37.c${chr} \
    --bed ${data_field}_c${chr}_v3_new_setting \
    --phenoFile fm_ffm_4regenie.phe \
    --covarFile fm_ffm_4regenie.phe \
    --phenoCol fat_mass --phenoCol fat_free_mass \
    --covarCol sex  --covarCol age --covarCol pc{1:10} \
    --catCovarList genotype_measurement_batch \
    --pred fm_ffm_regenie_37_results_new_setting_pred.list --bsize 200 \
    --threads 16 --maxCatLevels 120 --gz"

  dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_c${chr}_v3.bed" \
   -iin="${data_file_dir}/${data_field}_c${chr}_v3.bim" \
   -iin="${data_file_dir}/${data_field}_c${chr}_v3.fam" \
   -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_pred.list" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_1.loco.gz" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_2.loco.gz" \
   -icmd="${run_regenie_cmd}" --tag="Step2" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/output_path" --brief --yes

done


