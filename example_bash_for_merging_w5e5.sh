
startYear="1980"
endYear="1984"
inputDirRoot="/scratch-shared/vbonato/dynqual_simulations/w5e5/existing/1980-1984/"
outputDir="/scratch-shared/edwinaha/dynqual_simulations/w5e5/existing/1980-1984/global/"

sbatch --export inputDirRoot=${inputDirRoot},outputDir=${outputDir},startYear=${startYear},endYear=${endYear} slurm_job_for_merging.sh
