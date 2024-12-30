
startYear="2035"
endYear="2044"
inputDirRoot="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2035-2044/"
outputDir="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2035-2044/global/"

sbatch --export inputDirRoot=${inputDirRoot},outputDir=${outputDir},startYear=${startYear},endYear=${endYear} slurm_job_for_merging_spatially.sh
