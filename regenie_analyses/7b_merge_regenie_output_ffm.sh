data_file_dir="${project}:/output_path/"

merge_cmd='out_file="assoc.fat_mass_37.regenie.merged.txt"
     cp /mnt/project/output_path/assoc_37.*fat_mass.regenie.gz .
     gunzip *.regenie.gz

echo -e "CHROM\tGENPOS\tID\tALLELE0\tALLELE1\tA1FREQ\tN\tTEST\tBETA\tSE\tCHISQ\tLOG10P\tEXTRA" > $out_file

files="./*.regenie"
for f in $files
do
   tail -n+2 $f | tr " " "\t" >> $out_file
done

rm *.regenie ' 


dx run swiss-army-knife -iin="${data_file_dir}/assoc_37.c1_fat_mass.regenie.gz" \
   -icmd="${merge_cmd}" --tag="Step1" --instance-type "mem1_ssd1_v2_x16"\
   --destination="${project}:/output_path" --brief --yes 
