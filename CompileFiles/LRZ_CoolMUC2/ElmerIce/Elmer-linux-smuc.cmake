# CMake toolchain file for building with GNU compilers
#
# Author: Mikko Byckling, CSC - IT Center for Science Ltd.
# Version: 0.1

SET(CMAKE_SYSTEM_NAME Linux)

# Specify the cross compilers (serial)
SET(CMAKE_C_COMPILER icc)
SET(CMAKE_Fortran_COMPILER ifort)
SET(CMAKE_CXX_COMPILER icpc)
#SET(CMAKE_C_COMPILER gcc)
#SET(CMAKE_Fortran_COMPILER gfortran)
#SET(CMAKE_CXX_COMPILER g++)

# Specify the cross compilers (parallel)
SET(MPI_C_COMPILER mpiicc)
SET(MPI_CXX_COMPILER mpiicpc)
SET(MPI_Fortran_COMPILER mpiifort)
#SET(MPI_C_COMPILER mpicc)
#SET(MPI_CXX_COMPILER mpicxx)
#SET(MPI_Fortran_COMPILER mpif90)

# Compilation flags (i.e. with optimization)
SET(CMAKE_C_FLAGS "-fPIC -O3" CACHE STRING "")
SET(CMAKE_CXX_FLAGS "-fPIC -O3" CACHE STRING "")
SET(CMAKE_Fortran_FLAGS "-fPIC -O3 " CACHE STRING "")
#SET(CMAKE_C_FLAGS "-O3 -fp-model fast=2" CACHE STRING "")
#SET(CMAKE_CXX_FLAGS "-O3 -fp-model fast=2" CACHE STRING "")
#SET(CMAKE_Fortran_FLAGS "-O3 -fp-model fast=2" CACHE STRING "")


# Reset the default build type flags, all flags should be set above
SET(CMAKE_C_FLAGS_RELWITHDEBINFO "" CACHE STRING "")
SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO "" CACHE STRING "")
SET(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "" CACHE STRING "")
#SET(CMAKE_C_FLAGS "-O3 -xMIC-AVX512" CACHE STRING "")
#SET(CMAKE_CXX_FLAGS "-O3 -xMIC-AVX512" CACHE STRING "")
#SET(CMAKE_Fortran_FLAGS "-O3 -xMIC-AVX512" CACHE STRING "")

# MUMPS
SET(MUMPSROOT /home/hpc/a2901/di36hov/INSTALL/MumpsInstall/MUMPS_5.1.2/)
SET(MUMPSINCLUDE "${MUMPSROOT}/include")
SET(MUMPSLIB "${MUMPSROOT}/lib")

# Metis
#SET(METISROOT /lrz/sys/libraries/metis/5.1.0/i4r4)
#SET(METISINCLUDE "${METISROOT}/include")
#SET(METISLIB "${METISROOT}/lib")
#ADD_DEFINITIONS(-I/${METISINCLUDE} -L=${METISLIB})

# Hypre
#SET(HYPREROOT /home/hpc/a2901/di36hov/INSTALL/hypre-2.11.2/src/hypre)
#SET(HYPREINCLUDE "${HYPREROOT}/include")
#SET(HYPRELIB "${HYPREROOT}/lib")
#ADD_DEFINITIONS(-I/${HYPREINCLUDE} -L=${HYPRELIB})

SET(MMGROOT /home/hpc/a2901/di36hov/INSTALL/mmg-5.3.10/MMG_devel/)
SET(MMGINCLUDE "${MMGROOT}/include")
SET(MMGLIB "${MMGROOT}/lib")
ADD_DEFINITIONS(-I/${MMGINCLUDE} -L=${MMGLIB})
