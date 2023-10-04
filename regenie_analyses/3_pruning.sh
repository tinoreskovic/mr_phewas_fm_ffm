data_field="ukb22418"
data_file_dir="/mnt/project/output_path"
txt_file_dir="/mnt/project/output_path"

run_plink_qc="plink2 --bfile ${data_field}_allQC_v2_merged_phe \
 --indep-pairwise 1000 50 0.4  --out ukb-pruning ;\
ls *bed; \
plink2 --bfile ${data_field}_allQC_v2_merged_phe --extract ukb-pruning.prune.in \
 --keep phenotypes.v08-04-22.txt --make-bed --out ${data_field}_allQC_v2_mrg_prun_cohort ;\
wc *.bim "

dx run swiss-army-knife -iin="${data_file_dir}/${data_field}_allQC_v2_merged_phe.bed" \
   -iin="${data_file_dir}/${data_field}_allQC_v2_merged_phe.bim" \
   -iin="${data_file_dir}/${data_field}_allQC_v2_merged_phe.fam"\
   -iin="${txt_file_dir}/fm_ffm_4regenie_wes.phe" \
   -icmd="${run_plink_qc}" --tag="Step1" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/data/output_path_2/" --brief --yes
