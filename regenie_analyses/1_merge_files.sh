run_merge="cp /mnt/project/Bulk/Genotype\ Results/Genotype\ calls/ukb22418_c[1-9]* . ;\
        cp /mnt/project/Bulk/Genotype\ Results/Genotype\ calls/ukb22418_cX_* . ;\
        ls *.bed | sed -e 's/.bed//g'> files_to_merge.txt; \
        plink --merge-list files_to_merge.txt --make-bed\
        --out ukb22418_all_v2_merged;\
        rm files_to_merge.txt;\
        rm ukb22418_c*"

dx run swiss-army-knife -icmd="${run_merge}" --tag="Step1" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/output_path/" --brief --yes 
