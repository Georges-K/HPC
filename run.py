import os
n=3

print("********************************* \n")
print("Using Intel compiler, opimization set to : -O3 with Blas routine \n \n")
for i in range(n):
    os.system("ifort -O3 -o out%i.x mat_mat_blas.f90 fillMatrix.f90 matrix_mul.f90 -lblas" % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using Intel compiler, opimization set to : -O3 \n \n")
for i in range(n):
    os.system("ifort -O3 -o out%i.x mat_mat.f90 fillMatrix.f90 matrix_mul.f90" % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using Intel compiler, opimization set to : -O0 with Blas routine \n \n")
for i in range(n):
    os.system("ifort -O0 -o out%i.x mat_mat_blas.f90 fillMatrix.f90 matrix_mul.f90 -lblas" % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using Intel compiler, opimization set to : -O0 \n \n")
for i in range(n):
    os.system("ifort -O0 -o out%i.x mat_mat.f90 fillMatrix.f90 matrix_mul.f90" % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using GNU compiler, opimization set to : -O3 with Blas routine \n \n")
for i in range(n):
    os.system("gfortran -O3 -o out%i.x mat_mat_blas.f90 fillMatrix.f90 matrix_mul.f90 -lblas" % i)
    os.system("./out%i.x" % i )
    
print("********************************* \n")
print("Using GNU compiler, opimization set to : -O0  \n \n")
for i in range(n):
    os.system("gfortran -O0 -o out%i.x mat_mat.f90 fillMatrix.f90 matrix_mul.f90" % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using GNU compiler, opimization set to : -O3 \n \n")
for i in range(n):
    os.system("gfortran -O3 -o out%i.x mat_mat.f90 fillMatrix.f90 matrix_mul.f90 " % i)
    os.system("./out%i.x" % i )

print("********************************* \n")
print("Using GNU compiler, opimization set to : -O0 with Blas routine \n \n")
for i in range(n):
    os.system("gfortran -O0 -o out%i.x mat_mat_blas.f90 fillMatrix.f90 matrix_mul.f90 -lblas" % i)
    os.system("./out%i.x" % i )





