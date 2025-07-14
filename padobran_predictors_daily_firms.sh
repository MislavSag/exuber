#!/bin/bash

#PBS -N exuber_predictions
#PBS -l ncpus=1
#PBS -l mem=2GB
#PBS -J 1-9828
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors_daily.R
