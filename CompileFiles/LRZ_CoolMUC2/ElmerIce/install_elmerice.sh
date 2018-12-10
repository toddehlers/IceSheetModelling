#!/bin/bash
module unload intel
module load intel/17.0
module unload mpi.intel
module load mpi.intel/2017
module unload mkl
module load mkl/2017
module load spack/18.2
module load hypre/2.14.0-intel-impi
module load metis
module load parmetis


CMAKE=cmake

# Installation directory (set these!)
TIMESTAMP=$(date +"%m-%d-%y")
ELMER_REV="Elmer_devel_${TIMESTAMP}"
#ELMER_REV="Elmer_devel"
ELMERSRC="$HOME/INSTALL/InstallElmerv83WithMMGRecomp/elmerice"
BUILDDIR="$ELMERSRC/builddir"
IDIR="$ELMERSRC/$ELMER_REV"
mkdir -p ${BUILDDIR}
TOOLCHAIN="$HOME/INSTALL/InstallElmerv83WithMMGRecomp/elmerice/Elmer-linux-smuc.cmake"

echo "Building Elmer from within " ${BUILDDIR}
echo "using following toolchain file " ${TOOLCHAIN}
echo "installation into " ${IDIR}
cd ${BUILDDIR}
pwd
ls -ltr

echo $CMAKE $ELMERSRC
		#-DWITH_OpenMP:BOOL=TRUE \
		#-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
		#-DWITH_OpenMP:BOOL=TRUE \

$CMAKE $ELMERSRC  \
    -DCMAKE_INSTALL_PREFIX=$IDIR \
    -DCMAKE_TOOLCHAIN_FILE=$TOOLCHAIN \
		-DNetCDF_INCLUDE_DIR=/lrz/sys/io_tools/netcdf/4.3.3/serial_icc17/include \
    -DNetCDF_LIBRARY=/lrz/sys/io_tools/netcdf/4.3.3/serial_icc17/lib/libnetcdf.so \
    -DNetCDFF_LIBRARY=/lrz/sys/io_tools/netcdf/4.3.3/serial_icc17/lib/libnetcdff.so \
		-DCSA_LIBRARY=/home/hpc/a2901/di36hov/INSTALL/csa-c/csa/lib/libcsa.a \
    -DCSA_INCLUDE_DIR=/home/hpc/a2901/di36hov/INSTALL/csa-c/csa \
    -DNN_INCLUDE_DIR=/home/hpc/a2901/di36hov/INSTALL/nn-c/nn \
    -DNN_LIBRARY=/home/hpc/a2901/di36hov/INSTALL/nn-c/nn/lib/libnn.a \
		-DWITH_ScatteredDataInterpolator:BOOL=TRUE \
    -DWITH_MPI:BOOL=TRUE \
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

make && make install

###########################################################
#Testing ELmerice , go to builddir and write this command
#ctest -L elmerice
##########################################################
