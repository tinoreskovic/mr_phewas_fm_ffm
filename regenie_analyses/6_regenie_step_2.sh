imp_file_dir="/Bulk/Imputation/UKB imputation from genotype/"
data_field="ukb22828"
data_file_dir="${project}:/path/to/regenie_ukb_unfolded"
data_file_dir2="${project}:/data/gt_genrel_block"
txt_file_dir="${project}:/path/to/regenie_ukb_nonfolded"


for chr in {1..22}; do
  run_regenie_cmd="regenie --step 2 --out assoc_37.c${chr} \
    --bed ${data_field}_c${chr}_v3 \
    --phenoFile fm_ffm_4regenie.phe \
    --covarFile fm_ffm_4regenie.phe \
    --phenoCol fat_mass --phenoCol fat_free_mass \
    --covarCol sex  --covarCol age \
    --covarCol genotype_measurement_batch --covarCol pc{1:10} \
    --pred fm_ffm_regenie_37_results_nonpruned_pred.list --bsize 200 \
    --minMAC 3 --threads 16 --gz"

  dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_c${chr}_v3.bed" \
   -iin="${data_file_dir}/${data_field}_c${chr}_v3.bim" \
   -iin="${data_file_dir}/${data_field}_c${chr}_v3.fam" \
   -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_pred.list" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_1.loco.gz" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_2.loco.gz" \
   -icmd="${run_regenie_cmd}" --tag="Step2" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/path/to/regenie_ukb_unfolded" --brief --yes

done

# now run chrX

  run_regenieX_cmd="regenie --step 2 --out assoc_37.cX \
    --bed ${data_field}_cX_v3 \
    --phenoFile fm_ffm_4regenie.phe \
    --covarFile fm_ffm_4regenie.phe \
    --phenoCol fat_mass --phenoCol fat_free_mass \
    --covarCol sex  --covarCol age \
    --covarCol genotype_measurement_batch --covarCol pc{1:10} \
    --pred fm_ffm_regenie_37_results_nonpruned_pred.list --bsize 200 \
    --minMAC 3 --threads 16 --gz"

  dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_cX_v3.bed" \
   -iin="${data_file_dir}/${data_field}_cX_v3.bim" \
   -iin="${data_file_dir}/${data_field}_cX_v3.fam" \
   -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_pred.list" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_1.loco.gz" \
   -iin="${data_file_dir}/fm_ffm_regenie_37_results_nonpruned_2.loco.gz" \
   -icmd="${run_regenieX_cmd}" --tag="Step2" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/path/to/regenie_ukb_unfolded" --brief --yes

