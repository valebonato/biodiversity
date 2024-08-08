#!/bin/bash

#SBATCH -N 1
#SBATCH -n 32
#SBATCH -t 119:59:00
#SBATCH -p genoa
#SBATCH -J submit_merge_jobs
#SBATCH --output=submit_merge_jobs_%j.log

# Activate the correct conda environment and other settings
. /home/edwin/load_all_default.sh

# Define the categories and conditions
categories=("UKESM" "MRI")
conditions=("existing" "existing_future" "natural_condition")

# Initialize job counter
job_counter=0
max_jobs=20

# Function to submit a job for merging files
submit_job() {
    local category=$1
    local condition=$2
    sbatch --export=ALL,category=${category},condition=${condition} <<EOF
#!/bin/bash
#SBATCH -N 1
#SBATCH -n 32
#SBATCH -t 119:59:00
#SBATCH -p genoa
#SBATCH -J merge_nc_files_${category}_${condition}
#SBATCH --output=merge_nc_files_${category}_${condition}_%j.log
. /home/edwin/load_all_default.sh
bash merge_nc_files.sh ${category} ${condition}
EOF
}

# Function to submit jobs in batches
submit_jobs_in_batches() {
    local start_index=$1
    local end_index=$2

    # Loop through categories and conditions
    for ((i = start_index; i < end_index && i < ${#categories[@]} * ${#conditions[@]}; i++)); do
        category_index=$((i / ${#conditions[@]}))
        condition_index=$((i % ${#conditions[@]}))
        category=${categories[category_index]}
        condition=${conditions[condition_index]}
        
        # Submit the job
        submit_job ${category} ${condition}
    done
}

# Total number of combinations
total_combinations=$((${#categories[@]} * ${#conditions[@]}))

# Loop to submit jobs in batches
for ((batch_start = 0; batch_start < total_combinations; batch_start += max_jobs)); do
    batch_end=$((batch_start + max_jobs))
    echo "Submitting jobs from ${batch_start} to ${batch_end}"
    submit_jobs_in_batches ${batch_start} ${batch_end}

    # Wait for the current batch to finish
    wait
done

echo "All jobs submitted."
