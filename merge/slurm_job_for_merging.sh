#!/bin/bash

# we reserve one node of eejit, one contains 96 cores
#SBATCH -N 1

# we use all cores
#SBATCH -n 32

# wall clock time (maximum 120 hours)
#SBATCH -t 119:59:00

# the partition name 
#SBATCH -p genoa

# job name
#SBATCH -J merging

# exporting some variables
#SBATCH --export inputDirRoot="",outputDir="",startYear="",endYear=""


#~ inputDirRoot="/scratch-shared/vbonato/dynqual_simulations/w5e5/existing/1980-1984/"
#~ outputDir="/scratch-shared/edwinaha/dynqual_simulations/w5e5/existing/1980-1984/global/"
#~ startYear="1980"
#~ endYear="1984"


# we activate the correct conda environment and many other settings
. /home/edwin/load_all_default.sh

# daily resolution
python merge_netcdf_general.py ${inputDirRoot} ${outputDir} "outDailyTotNC" ${startYear}"-01-01" ${endYear}"-12-31" discharge,waterTemp NETCDF4 True 2 M01,M02,M03,M05,M06,M07,M08,M09 defined 300 -17 -35 52 38 &

# monthly resolution
python merge_netcdf_general.py ${inputDirRoot} ${outputDir} "outMonthAvgNC" ${startYear}"-01-31" ${endYear}"-12-31" discharge,waterTemp NETCDF4 True 2 M01,M02,M03,M05,M06,M07,M08,M09 defined 300 -17 -35 52 38 &

wait
