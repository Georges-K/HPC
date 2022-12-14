#!/bin/bash
#SBATCH --job-name=HPCAssignment3
#SBATCH --mem=0
#SBATCH --ntasks=16
#SBATCH --time=01:00:00
#SBATCH --account=def-piromh 
#SBATCH --mail-user=georges.karagozian@ontariotechu.net
#SBATCH --mail-type=ALL
#SBATCH -o out_%A_%a.out # Standard output
#SBATCH -e out_%A_%a.err # Standard error

mpirun -np 16 ./test.x
