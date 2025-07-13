#!/bin/bash

#PBS -N exuber_predictions
#PBS -l ncpus=1
#PBS -l mem=2GB
#PBS -J 1-24223
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors.R
