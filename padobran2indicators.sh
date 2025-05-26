#!/bin/bash

#PBS -N indicators
#PBS -l ncpus=1
#PBS -l mem=130GB
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran2indicators.R
