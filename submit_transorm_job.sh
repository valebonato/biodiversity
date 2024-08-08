#!/bin/bash

#SBATCH -N 1
#SBATCH -n 32
#SBATCH -t 119:59:00
#SBATCH -p genoa
#SBATCH -J transform_to_weekly
#SBATCH --output=transform_to_weekly_%j.log

# Activate the correct conda environment and other settings
. /home/edwin/load_all_default.sh

# Define the input and output directories
inputDirRoot="/projects/0/prjs0992/vbonato1"
outputDirRoot="/projects/0/prjs0992/vbonato1/weekly_data"

# Define the categories and conditions
categories=("UKESM")
conditions=("existing" "existing_future" "natural_condition")

# Loop through categories and conditions
for category in "${categories[@]}"; do
    for condition in "${conditions[@]}"; do
        inputDir="${inputDirRoot}/${category}/${condition}/merged"
        outputDir="${outputDirRoot}/${category}/${condition}/merged"
        
        # Submit the transformation job
        sbatch --export=ALL,inputDir=${inputDir},outputDir=${outputDir} <<EOF
#!/bin/bash
#SBATCH -N 1
#SBATCH -n 32
#SBATCH -t 119:59:00
#SBATCH -p genoa
#SBATCH -J transform_to_weekly_${category}_${condition}
#SBATCH --output=transform_to_weekly_${category}_${condition}_%j.log
. /home/edwin/load_all_default.sh
bash transform_to_weekly.sh ${inputDir} ${outputDir}
EOF
    done
done
