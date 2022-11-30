import os
import numpy as np

nthread = np.array([1,4,8,16])
npart = np.array([50,100,500,1000])
nsec = np.array([4,6])
i = 0
walltime = open("WallTime.txt", "w")
walltime.close()

for n in npart:
    for thread in nthread:
        for secs in nsec:
            input_file = open("input.txt", "w")
            input_file.write("%d %d %d" % (n, secs, thread))
            input_file.close()
            os.system("ifort -O3 LangDyn_2D_Inter_DD_omp.f90 -o out%i.x -fopenmp" % i)
            os.system("./out%i.x" % i )
            i += 1