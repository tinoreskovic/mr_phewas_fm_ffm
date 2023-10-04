imp_file_dir="/Bulk/Imputation/UKB imputation from genotype/"
data_field="ukb22828"
txt_file_dir="${project}:/path/to/regenie_ukb_nonfolded"

for i in {1..22}; do
    run_plink_imp="plink2 --bgen ${data_field}_c${i}_b0_v3.bgen ref-first\
      --sample ${data_field}_c${i}_b0_v3.sample \
      --make-pgen --out ukbi_ch${i}_v3; \
    plink2 --pfile ukbi_ch${i}_v3 \
      --no-pheno --keep fm_ffm_4regenie.phe \
      --maf 0.01 --mac 20 --geno 0.1 --hwe 1e-15 --mind 0.1 \
      --make-bed --out ${data_field}_c${i}_v3; \
     rm ukbi_ch${i}_v3* "

    dx run swiss-army-knife -iin="${imp_file_dir}/${data_field}_c${i}_b0_v3.bgen" \
     -iin="${imp_file_dir}/${data_field}_c${i}_b0_v3.sample" \
     -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
     -icmd="${run_plink_imp}" --tag="Step2" --instance-type "mem2_ssd2_v2_x16"\
     --destination="${project}:/path/to/regenie_ukb_unfolded" --brief --yes
done

# now run chrX

    run_plink_impX="plink2 --bgen ${data_field}_cX_b0_v3.bgen ref-first\
      --sample ${data_field}_cX_b0_v3.sample \
      --make-pgen --out ukbi_chX_v3; \
    plink2 --pfile ukbi_chX_v3 \
      --no-pheno --keep fm_ffm_4regenie.phe \
      --maf 0.01 --mac 20 --geno 0.1 --hwe 1e-15 --mind 0.1 \
      --make-bed --out ${data_field}_cX_v3; \
     rm ukbi_chX_v3* "
    
    dx run swiss-army-knife -iin="${imp_file_dir}/${data_field}_cX_b0_v3.bgen" \
     -iin="${imp_file_dir}/${data_field}_cX_b0_v3.sample" \
     -iin="${txt_file_dir}/fm_ffm_4regenie.phe" \
     -icmd="${run_plink_impX}" --tag="Step2" --instance-type "mem2_ssd2_v2_x16"\
     --destination="${project}:/path/to/regenie_ukb_unfolded" --brief --yes


