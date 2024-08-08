
startYear="2045"
endYear="2050"
inputDirRoot="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2045-2050/"
outputDir="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2045-2050/global/"

sbatch --export inputDirRoot=${inputDirRoot},outputDir=${outputDir},startYear=${startYear},endYear=${endYear} slurm_job_for_merging_spatially.sh
