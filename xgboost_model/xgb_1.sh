#!/bin/bash
#SBATCH -n 28# SBATCH -N 1 
#SBATCH -o athena_%j.out
 #SBATCH -e athena_%j.err 
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=jerson_cochancela@brown.edu 
R CMD BATCH ~/ML/xgb1.R