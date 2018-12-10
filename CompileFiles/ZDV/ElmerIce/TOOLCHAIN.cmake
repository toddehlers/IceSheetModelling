# Make toolchain file for building with GNU compilers
#
# Author: Mikko Byckling, CSC - IT Center for Science Ltd.
# Version: 0.1

SET(CMAKE_SYSTEM_NAME Linux)
SET(CMAKE_SYSTEM_PROCESSOR x86_64)
SET(CMAKE_SYSTEM_VERSION 1)

# Specify the cross compilers (serial)
SET(CMAKE_C_COMPILER icc)
SET(CMAKE_Fortran_COMPILER ifort)
SET(CMAKE_CXX_COMPILER icpc)

# Specify the cross compilers (parallel)
SET(MPI_C_COMPILER mpiicc)
SET(MPI_CXX_COMPILER mpiicpc)
SET(MPI_Fortran_COMPILER mpiifort)

#SET(CMAKE_C_FLAGS "-qopenmp" CACHE STRING "" )
#SET(CMAKE_CXX_FLAGS "-qopenmp" CACHE STRING "" )
#SET(CMAKE_Fortran_FLAGS "-qopenmp" CACHE STRING "" )

# MUMPS
SET(MUMPSROOT /home-link/epioi01/INSTALL/MUMPS_5.1.2)
#SET(MUMPSROOT /beegfs/home/tu/epioi01/INSTALL/MUMPS_5.1.2)
SET(MUMPSINCLUDE "${MUMPSROOT}/include")
SET(MUMPSLIB "${MUMPSROOT}/lib")
#SET(Mumps_INCLUDE_DIR ${MUMPSINCLUDE})
#SET(Mumps_LIB_DIR ${MUMPSLIB})
#SET(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${MUMPSLIB})
#ADD_DEFINITIONS(-I${MUMPSINCLUDE} -L=${MUMPSLIB})
#ADD_DEFINITIONS(-ldmumps)
#ADD_DEFINITIONS(-lmumps_common)
#ADD_DEFINITIONS(-lpord)

#SET(MMGROOT /home-link/epioi01/INSTALL/mmg-5.2.6/MMG_devel/)
SET(MMGROOT /home-link/epioi01/INSTALL/mmg-5.3.10/MMG_devel/)
SET(MMGINCLUDE "${MMGROOT}/include")
SET(MMGLIB "${MMGROOT}/lib")
ADD_DEFINITIONS(-I/${MMGINCLUDE} -L=${MMGLIB})
