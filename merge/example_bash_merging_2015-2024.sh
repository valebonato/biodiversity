
startYear="2015"
endYear="2024"
inputDirRoot="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2015-2024/"
outputDir="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2015-2024/global/"

sbatch --export inputDirRoot=${inputDirRoot},outputDir=${outputDir},startYear=${startYear},endYear=${endYear} slurm_job_for_merging_spatially.sh
