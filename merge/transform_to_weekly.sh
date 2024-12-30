#!/bin/bash

# Define input and output directories
inputDir=$1
outputDir=$2

# Create the output directory if it doesn't exist
mkdir -p $outputDir

# List of files to process
files=("discharge_dailyTot_output_2015-2050.nc" "waterTemp_dailyTot_output_2015-2050.nc")

# Loop through the files and process them
for file in "${files[@]}"; do
    inputFile="${inputDir}/${file}"
    outputFile="${outputDir}/${file/dailyTot_output/weeklyTot_output}"
    
    # Check if input file exists
    if [ -f "$inputFile" ]; then
        # Convert daily data to weekly data
        cdo -b 32 timselmean,7 $inputFile $outputFile
        echo "Converted $inputFile to $outputFile"
    else
        echo "File $inputFile does not exist"
    fi
done
