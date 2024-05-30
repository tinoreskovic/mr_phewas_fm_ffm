imp_file_dir="/Bulk/Imputation/UKB imputation from genotype/"
data_field="ukb22828"
txt_file_dir="${project}:/output_path"

for i in {1..22}; do
    run_plink_imp="plink2 --bgen ${data_field}_c${i}_b0_v3.bgen ref-first\
      --sample ${data_field}_c${i}_b0_v3.sample \
      --make-pgen --out ukbi_ch${i}_v3; \
    plink2 --pfile ukbi_ch${i}_v3_new_setting \
      --no-pheno --keep fm_ffm_4regenie.phe \
      --maf 0.01 --hwe 1e-15 --geno 0.01 --snps-only --chr 1-22 --max-alleles 2 \
      --make-bed --out ${data_field}_c${i}_v3_new_setting; \
     rm ukbi_ch${i}_v3* "

    dx run swiss-army-knife -iin="${imp_file_dir}/${data_field}_c${i}_b0_v3.bgen" \
     -iin="${imp_file_dir}/${data_field}_c${i}_b0_v3.sample" \
     -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
     -icmd="${run_plink_imp}" --tag="Step2" --instance-type "mem2_ssd2_v2_x16"\
     --destination="${project}:/output_path" --brief --yes
done




