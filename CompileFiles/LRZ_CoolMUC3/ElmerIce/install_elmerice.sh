#!/bin/bash
#module unload intel
#module load intel/17.0
#module unload mpi.intel
#module load mpi.intel/2017
#module unload mkl
#module load mkl/2017
#module load spack/18.2
#module load cmake/3.6
#module load mumps/5.1.1-intel-impi
#module load hypre/2.14.0-intel-impi
#module load metis/5.1.0-intel-i32-r32
#module load parmetis/4.0.3-intel-impi-i32-r32
module load spack
module load hypre/2.14.0-intel-impi
module load metis/5.1.0-intel-i32-r32
module load parmetis/4.0.3-intel-impi-i32-r32


CMAKE=cmake

# Installation directory (set these!)
TIMESTAMP=$(date +"%m-%d-%y")
ELMER_REV="Elmer_devel_${TIMESTAMP}_mpp3_mumps5_1_2"
#ELMER_REV="Elmer_devel"
ELMERSRC="/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83_LRZ/elmerice"
BUILDDIR="$ELMERSRC/builddir_mpp3"
IDIR="$ELMERSRC/$ELMER_REV"
mkdir -p ${BUILDDIR}
TOOLCHAIN="${ELMERSRC}/Elmer-linux-smuc.cmake"

echo "Building Elmer from within " ${BUILDDIR}
echo "using following toolchain file " ${TOOLCHAIN}
echo "installation into " ${IDIR}
cd ${BUILDDIR}
pwd
ls -ltr

echo $CMAKE $ELMERSRC

$CMAKE $ELMERSRC  \
    -DCMAKE_INSTALL_PREFIX=$IDIR \
    -DCMAKE_TOOLCHAIN_FILE=$TOOLCHAIN \
		-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
    -DWITH_MPI:BOOL=TRUE \
		-DWITH_OpenMP:BOOL=TRUE \
    -DWITH_Mumps:BOOL=TRUE \
    -DWITH_Hypre:BOOL=TRUE \
    -DWITH_Trilinos:BOOL=FALSE \
    -DWITH_ELMERGUI:BOOL=FALSE \
    -DWITH_ElmerIce:BOOL=TRUE \
    -DWITH_MKL:BOOL=TRUE \
    -DWITH_OCC:BOOL=TRUE \
    -DWITH_QWT:BOOL=TRUE \
    -DWITH_VTK:BOOL=FALSE \
    -DWITH_PARAVIEW:BOOL=FALSE

#make && make install

###########################################################
#Testing ELmerice , go to builddir and write this command
#ctest -L elmerice
##########################################################
