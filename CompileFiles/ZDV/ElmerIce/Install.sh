## NUR FUER ELMER
source /etc/profile.d/modules.sh
module load chains/INTEL-17.0
module load compiler/intel/17.0
module load mpi/impi/2017
module load numlib/mkl/2017.6.256
module load lib/netcdf/4.4.1-intel-17.0
##Get Elmer/Ice
##git clone git://www.github.com/ElmerCSC/elmerfem -b elmerice elmerice

CMAKE=cmake

## Installation directory (set these!)
TIMESTAMP=$(date +"%m-%d-%y")
ELMER_REV="Elmer_devel_${TIMESTAMP}"
ELMERSRC="/home-link/epioi01/INSTALL/InstallElmerv83WithMMG/"
BUILDDIR="$ELMERSRC/build"
IDIR="/home-link/epioi01/INSTALL/InstallElmerv83WithMMG/$ELMER_REV"
TOOLCHAIN="/home-link/epioi01/INSTALL/InstallElmerv83WithMMG/TOOLCHAIN.cmake"
## next line is optional
##TOOLCHAIN="/path/to/your/own/toolchainfile/yourtoolchainfile.cmake"
#
echo "Building Elmer from within " ${BUILDDIR}
echo "using following toolchain file " ${TOOLCHAIN}
echo "installation into " ${IDIR}
mkdir -p ${BUILDDIR}
cd ${BUILDDIR}
pwd
ls -ltr
#
echo $CMAKE $ELMERSRC
##set LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/4.9/:$LD_LIBRARY_PATH
## you can add a -DCMAKE_TOOLCHAIN_FILE=$TOOLCHAIN,
## if you have a toolchain file declared
		#-DCMAKE_BUILD_TYPE=Debug \
		#-DNetCDF_INCLUDE_DIR=/opt/bwhpc/common/lib/netcdf/4.4.1-intel-17.0/include \
$CMAKE $ELMERSRC  \
		-DCMAKE_TOOLCHAIN_FILE=$TOOLCHAIN \
		-DCMAKE_INSTALL_PREFIX=$IDIR \
		-DNetCDF_INCLUDE_DIR=/home-link/epioi01/INSTALL/netcdf4.4.1/netcdf-fortran-4.4.1/build/include \
		-DNetCDF_LIBRARY=/opt/bwhpc/common/lib/netcdf/4.4.1-intel-17.0/lib/libnetcdf.so \
		-DNetCDFF_LIBRARY=/home-link/epioi01/INSTALL/netcdf4.4.1/netcdf-fortran-4.4.1/build/lib/libnetcdff.so \
    -DCSA_LIBRARY=/home-link/epioi01/INSTALL/csa-c/csa/lib/libcsa.a \
    -DCSA_INCLUDE_DIR=/home-link/epioi01/INSTALL/csa-c/csa \
	  -DNN_INCLUDE_DIR=/home-link/epioi01/INSTALL/nn-c/nn \
	  -DNN_LIBRARY=/home-link/epioi01/INSTALL/nn-c/nn/lib/libnn.a \
		-DWITH_ScatteredDataInterpolator:BOOL=TRUE \
		-DWITH_MPI:BOOL=TRUE \
		-DWITH_Mumps:BOOL=TRUE \
		-DWITH_Hypre:BOOL=FALSE\
		-DWITH_Trilinos:BOOL=FALSE \
		-DWITH_ELMERGUI:BOOL=FALSE \
		-DWITH_ElmerIce:BOOL=TRUE \
		-DWITH_MKL:BOOL=TRUE \
		-DWITH_OCC:BOOL=TRUE \
		-DWITH_QWT:BOOL=TRUE
make && make install
    #-DCMAKE_C_COMPILER=/opt/bwhpc/common/compiler/intel/2017u1/compilers_and_libraries/linux/bin/intel64/icc\
    #-DCMAKE_Fortran_COMPILER=/opt/bwhpc/common/compiler/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort\

## setenv ELMER_HOME /home/hpc/a2901/di49sux/software/elmerice/Elmer_devel_06-14-16
## setenv ELMER_SOLVER_HOME /home/hpc/a2901/di49sux/software/elmerice/Elmer_devel_06-14-16/share/elmersolver/
##ctest -L elmerice-fast
