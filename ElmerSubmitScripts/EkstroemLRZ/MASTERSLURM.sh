#!/bin/bash
#SBATCH -o PATHSLURMFILES/SLURM_job.%j.%N.out
#SBATCH -e PATHSLURMFILES/SLURM_job.%j.%N.err
#SBATCH -D PATHSLURMFILES/
#SBATCH -J JOBNAME
#SBATCH --get-user-env
#SBATCH --nodes=NOOFNODES
#SBATCH --ntasks=TOTPROCS
#SBATCH --ntasks-per-node=PROCSPERNODE
#SBATCH --mail-type=BEGIN
#SBATCH --mail-user=EMAILADDRESS
#SBATCH --export=NONE
#SBATCH --time=RUNTIME
#SBATCH --clusters=mpp2
#source /etc/profile.d/modules.sh
#=================================================================================================================
module unload python
module load python
module unload intel
module load intel/17.0
module unload mpi.intel
module load mpi.intel/2017
module unload mkl
module load mkl/2017
module unload gcc
module load scalapack
module load mumps/serial/5.0.1
module load metis/5.1_i4r4
module load netcdf/serial/4.4

export ELMER_HOME="/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83WithMMG/elmerice/Elmer_devel_07-09-18"
export ELMER_SOLVER_HOME="$ELMER_HOME/bin"

export PATH=/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83WithMMG/elmerice/Elmer_devel_07-09-18/bin:$PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83WithMMG/elmerice/Elmer_devel_07-09-18/share/elmersolver/lib/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83WithMMG/elmerice/Elmer_devel_07-09-18/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/InstallElmerv83WithMMG/elmerice/Elmer_devel_07-09-18/lib/elmersolver/:$LD_LIBRARY_PATH

export PATH=/lrz/sys/io_tools/netcdf/4.4.1/intel/impi_2017/lib/libnetcdf.so:$PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/mmg-5.3.10/MMG_devel/lib:$LD_LIBRARY_PATH

export PATH=/home/hpc/a2901/di36hov/INSTALL/mmg-5.3.10/MMG_devel/bin:$PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/mmg-5.3.10/MMG_devel/lib:$LD_LIBRARY_PATH

export PATH=/home/hpc/a2901/di36hov/INSTALL/hypre-2.11.2/src/hypre/bin:$PATH
export LD_LIBRARY_PATH=/home/hpc/a2901/di36hov/INSTALL/hypre-2.11.2/src/hypre/lib:$LD_LIBRARY_PATH

export LD_LIBRARY_PATH=/lrz/sys/libraries/mumps/5.0.1/lib:$LD_LIBRARY_PATH
export PATH=/home/hpc/a2901/di36hov/INSTALL/gmsh-2.12.0-source/build_16022018:$PATH
#=================================================================================================================

#echo $SLURM_SUBMIT_DIR
echo Here comes the Nodelist:
echo $SLURM_JOB_NODELIST

echo Here comes the partition the job runs in:
echo $SLURM_JOB_PARTITION
cd $SLURM_SUBMIT_DIR

cp $ELMER_HOME/share/elmersolver/lib/FreeSurfaceSolver.so src/MyFreeSurfaceSolver.so
make compile
make ini
make grid
make submit



