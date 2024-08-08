
startYear="2025"
endYear="2034"
inputDirRoot="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2025-2034/"
outputDir="/scratch-shared/vbonato1/dynqual_simulations_5-8.5/MRI/natural_condition/2025-2034/global/"

sbatch --export inputDirRoot=${inputDirRoot},outputDir=${outputDir},startYear=${startYear},endYear=${endYear} slurm_job_for_merging_spatially.sh
