#!/bin/bash

# Define input variables
outputDirRootBase="/scratch-shared/vbonato1/dynqual_simulations_5-8.5"

# Function to unify the files
merge_files() {
    local category=$1
    local condition=$2

    # Create output directory
    outputDir="${outputDirRootBase}/${category}/${condition}/merged"
    mkdir -p ${outputDir}

    # Unify discharge daily files
    cdo cat ${outputDirRootBase}/${category}/${condition}/*/global/discharge_dailyTot_output_*.nc ${outputDir}/discharge_dailyTot_output_2015-2050.nc

    # Unify discharge monthly files
    cdo cat ${outputDirRootBase}/${category}/${condition}/*/global/discharge_monthAvg_output_*.nc ${outputDir}/discharge_monthAvg_output_2015-2050.nc

    # Unify waterTemp daily files
    cdo cat ${outputDirRootBase}/${category}/${condition}/*/global/waterTemp_dailyTot_output_*.nc ${outputDir}/waterTemp_dailyTot_output_2015-2050.nc

    # Unify waterTemp monthly files
    cdo cat ${outputDirRootBase}/${category}/${condition}/*/global/waterTemp_monthAvg_output_*.nc ${outputDir}/waterTemp_monthAvg_output_2015-2050.nc

    echo "Merged files for ${category}/${condition} into ${outputDir}"
}

# Read category and condition from arguments
category=$1
condition=$2

# Call the merge_files function
merge_files $category $condition
