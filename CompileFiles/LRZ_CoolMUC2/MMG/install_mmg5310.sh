#!/bin/bash

CMAKE=cmake

TIMESTAMP=$(date +"%m-%d-%y")
MMG_INSTALL=$"MMG_devel"
MMGSRC="$HOME/INSTALL/mmg-5.3.10"
BUILDDIR="$MMGSRC/build"
IDIR="$MMGSRC/$MMG_INSTALL"
mkdir -p ${BUILDDIR}
cd ${BUILDDIR}

$CMAKE $MMGSRC  \
    -DCMAKE_INSTALL_PREFIX=$IDIR \
    -DUSE_SCOTCH=OFF \
    -DCMAKE_C_COMPILER=icc \
    -DCMAKE_CXX_COMPILER=icpc \
	  -DCMAKE_Fortran_COMPILER=ifort \
    -DLIBMMG2D_SHARED:BOOL=ON \
    -DLIBMMG2D_STATIC:BOOL=ON \
    -DLIBMMG3D_SHARED:BOOL=ON \
    -DLIBMMG3D_STATIC:BOOL=ON \
    -DLIBMMGS_SHARED:BOOL=ON \
    -DLIBMMGS_STATIC:BOOL=ON \
    -DLIBMMG_SHARED:BOOL=ON \
    -DLIBMMG_STATIC:BOOL=ON

make && make install
