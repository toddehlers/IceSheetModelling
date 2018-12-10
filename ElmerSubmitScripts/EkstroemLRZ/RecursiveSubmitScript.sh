#!/bin/bash
###############################################################################
# This script recursively submits job script to the LRZ SLURM scheduler to be
# run until the specified simulation length is reached. All output is written
# including mesh files are written to $SCRATCH
# Input Parameters for the simulations have to be specified in the section below
# before the submission script starts (you can search for the start of the input
# parameter section with /SINPUT and likewise for the end of the input parameter
# section /EINPUT)
###############################################################################
shopt -s expand_aliases # this is necessary to ensure that aliases are expanded
#in the jobscript (In my case this is the listjob alias
source $HOME/.bashrc # load all your aliases. In my case they are in .bashrc
###############################################################################
#### MAKEFILE VARIABLES #######################################################
###############################################################################
#SINPUT
ProcessorNumber=80
Procx=8
Procy=10
sifBase=SubmitEkstroem4KMLGM
#sifMaster=MASTER.sif
sifMaster=MASTER_EkstroemLGM.sif
###############################################################################
#### END MAKEFILE VARIABLES ###################################################
###############################################################################
#### SIFFILE VARIABLES ########################################################
###############################################################################
VertMeshLevels=10 # number of vertical extrusion layers in siffile
InitTime=0 # makes sure that any previous relaxation simulations are deducted
#from perturbation simulation. This is obsolote if timer in Elmer is set to zeros
#at the start of perturbation simulation
TotalSimLength=20000 # in years
IndSimLength=200 # in years
TimeStepSize=0.5 # in years
WriteOutput=20 # in number of timesteps aka TimeStepSize * WriteOutput gives
#years
#RestartInit=InitialEkstroemLGMMMG10LBeta1e-1 # base name of the restart file from the relaxation
RestartInit=EkstroemLGM4KM10L52 # base name of the restart file from the relaxation
#simulation
#Path2RestartInit=/home/hpc/a2901/di36hov/EkstroemLGM/Shelf_HardBedLinearBMB/MMGMesh #
Path2RestartInit=/gpfs/scratch/a2901/di36hov/Shelf_HardBedLinearBMB/Files2600Years2800Years/FilesRun2/MMGMesh #
#Path to files for the first restart e.g. from a relaxation simulation
OutputFileBase=EkstroemLGM4KM10L6 # base name of output and restart file
SaveDirScratch=$SCRATCH/${PWD##*/} # takes name of current Dir and save
#simulation output to the same Dir just on $SCRATCH
TotalSimLength=$(($TotalSimLength+$InitTime))
#echo $SaveDirScratch
###############################################################################
#### END SIFFILE VARIABLES ####################################################
###############################################################################
###############################################################################
#### JOBFILE VARIABLES ########################################################
###############################################################################
Path4SLURMFiles=$PWD # Path to where job output is written
JobName=${PWD##*/} #Jobname is the same as the name of the submit Dir
TasksPerNode=28 #normally number of cores per node
#Email=Clemens.Schannwell@uni-tuebingen.de # for email notification when job
#starts. Comment out if no email notification is desired
#Runtime=06:15:00 #format has to be HH:MM:SS
Runtime=06:35:00 #format has to be HH:MM:SS
NoOfNodes=3 # Number of nodes on SLURM this is
#NoOfNodes=$(($ProcessorNumber/28)) # Number of nodes on SLURM this is
#TotProcs/28
SlurmMasterName=MASTERSLURM.sh
SlurmSubmitName=Submit.sh
YearCount=0
SimCounter=0 # counter for the number of simulations to allow for separate
#Directories for each of the simulation. Dunno if this is helpful. Might be a bit
#more organised.
echo $NoOfNodes
#EINPUT
###############################################################################
#### END JOBFILE VARIABLES ####################################################
###############################################################################
###############################################################################
#### START OF RECURSIVE SUBMIT SCRIPT #########################################
###############################################################################
### Create Output Directory on $SCRATCH #######################################
mkdir -p ${SaveDirScratch}
mkdir -p ${SaveDirScratch}/Init/MMGMesh
###############################################################################
### Copy initial restartfile to Output Dir on $SCRATCH ########################
cp -r $Path2RestartInit/$RestartInit*result* ${SaveDirScratch}/Init/MMGMesh
### Start submission loop #####################################################
while [ "$YearCount" -lt "$TotalSimLength" ]
do
		SimCounter=$(($SimCounter+1)) # increment counter
		echo This is $SimCounter Simulation
##############################################################################
### Replace Variable strings in SLURM job file ###############################
##############################################################################
		cp $SlurmMasterName $SlurmSubmitName
		if [ -n "${Email+set}" ]; then
				sed -i 's|EMAILADDRESS|'$Email'|g' $SlurmSubmitName
		else
				sed -i '/mail/d' $SlurmSubmitName
		fi
		sed -i 's|PATHSLURMFILES|'$Path4SLURMFiles'|g' $SlurmSubmitName
		sed -i 's|JOBNAME|'$JobName'|g' $SlurmSubmitName
		sed -i 's|NOOFNODES|'$NoOfNodes'|g' $SlurmSubmitName
		sed -i 's|TOTPROCS|'$ProcessorNumber'|g' $SlurmSubmitName
		sed -i 's|PROCSPERNODE|'$TasksPerNode'|g' $SlurmSubmitName
		sed -i 's|RUNTIME|'$Runtime'|g' $SlurmSubmitName
##############################################################################
### Finished replacing variable strings in SLURM job file ####################
##############################################################################
### Start replacing variable strings in Makefile and siffile #################
##############################################################################
		sif=$sifBase${SimCounter}.sif
		cp $sifMaster $sif
		cp MASTERMAKEFILE_LGM Makefile
		RestartFile=$OutputFileBase$(($SimCounter-1))
		OutputFile=$OutputFileBase${SimCounter}
		if [ "$SimCounter" == 1 ]; then
				sed -i 's|RESTARTFILE|'$RestartInit'|g' $sif
				sed -i 's|OUTPUTFILEVTU|'$OutputFile'|g' $sif
				sed -i "s|OUTPUTFILE|$OutputFile|g" $sif
				sed -i 's|MESHPATH|'$SaveDirScratch/Init'|g' $sif
				sed -i 's|MESHPATH|'$SaveDirScratch/Init'|g' Makefile
				echo $sif
		else
				sed -i 's|RESTARTFILE|'$RestartFile'|g' $sif
				sed -i 's|OUTPUTFILEVTU|'$OutputFile'|g' $sif
				sed -i "s|OUTPUTFILE|$OutputFile|g" $sif
				sed -i 's|MESHPATH|'$SaveFileDir'|g' $sif
				sed -i 's|MESHPATH|'$SaveFileDir'|g' Makefile
				sed -i '/Restart Time/d' $sif
				sed -i '/Equals dsdt/d' $sif
		fi
		sed -i "s/MESHLEV/$VertMeshLevels/g" $sif
		sed -i "s/INDSIMLEN/$IndSimLength/g" $sif
		sed -i "s/WRITEFILESTEPS/$WriteOutput/g" $sif
		sed -i "s/STEPSIZE/$TimeStepSize/g" $sif
		sed -i "s/ELEVBMBTIME/$TimeElevBMB/g" $sif
		sed -i "s/BMBMULT/$BMBMultiplier/g" $sif
		sed -i "s/TOTPROCS/$ProcessorNumber/g" Makefile
		sed -i "s/PROCX/$Procx/g" Makefile
		sed -i "s/PROCY/$Procy/g" Makefile
		sed -i "s/RES/$meshres/g" Makefile
		sed -i "s/SIFFILE/$sif/g" Makefile
##############################################################################
### Finished replacing variable strings in Makefile and siffile ##############
##############################################################################
### Move restart file to the next directory for the next simulation ##########
##############################################################################
		if [ "$SimCounter" == 2 ]; then
		#cp -r	$SaveDirScratch/Init/Outline/$OutputFileBase*result* $SaveFileDir/Outline/
		mv	$SaveDirScratch/Init/MMGMesh/$OutputFileBase*result* $SaveFileDir/MMGMesh/
		elif [ $SimCounter -gt 2 ]; then
		#cp -r $SaveDirScratch/FilesRun$(($SimCounter-2))/Outline/$OutputFileBase*$(($SimCounter-1))*result* $SaveDirScratch/FilesRun$(($SimCounter-1))/Outline/
		mv $SaveDirScratch/FilesRun$(($SimCounter-2))/MMGMesh/$OutputFileBase*$(($SimCounter-1))*result* $SaveDirScratch/FilesRun$(($SimCounter-1))/MMGMesh/
		fi
##############################################################################
### Start submitting the generated File to the SLURM scheduler ###############
##############################################################################
		JobSub=$(sbatch --parsable $SlurmSubmitName)
		JOBID=${JobSub%;*}
		JobActive=$(listjob | grep $JOBID)
		echo $JobActive
		while [ -n "$JobActive" ]; do
						sleep 10m
						JobActive=$(listjob | grep $JOBID)
						echo $JobActive
		done
##############################################################################
### Finished submitting the generated File to the SLURM scheduler ############
##############################################################################
##############################################################################
### Calculate from SLURM Output if more simulation are required if yes, start
### form the top #############################################################
##############################################################################
		YearCount=$(cat SLURM_job*$JOBID*.out | grep Time: | tail -n 1 | awk '{print $NF}')
		echo Yearcount before rounding with BC: $YearCount
		YearCount=$( bc <<< "($YearCount+0.49)/1")
		echo Yearcount after rounding with BC: $YearCount
##############################################################################
### Compute New Runtime for next Job #########################################
##############################################################################
		CPUTime=$(cat SLURM_job*$JOBID*.out | grep 'SOLVER TOTAL' | awk '{print $NF}')
		#if [ -n "${CPUTime+set}" ]; then
		if [ -z "$CPUTime" ]; then
		  echo Darn the job did not finish ....
			echo Exiting script. Please adjust Runtime manually in script ...
			exit 1
			#echo Increasing walltime for next job
      #CPUSec=$(echo $Runtime | awk -F':' '{print $1 * 60 * 60 + $2 * 60 + $3}')
			#echo $CPUSec
			#CPUTimeAdj=$(echo $CPUSec + $CPUSec*0.5 | bc)
			#echo $CPUTimeAdj
      #minutes=`echo "($CPUTimeAdj)/60" | bc`
			#hours=`echo "($minutes)/60" | bc`
			#minutes=`echo "($minutes)%60" | bc`
			#Runtime=${hours}:${minutes}:00
		else
		  echo Yay job finished within specified walltime!
			echo Calculating walltime of next job ...
      #CPUSec=$(echo $Runtime | awk -F':' '{print $1 * 60 * 60 + $2 * 60 + $3}')
			echo $CPUTime
			CPUBuffer=$(echo $CPUTime*0.3 | bc)
			echo $CPUBuffer
			if (( $(echo "$CPUBuffer > 3600" | bc -l) )); then
        echo CPUBuffer larger than 60 minutes ...
				echo Increasing Walltime by 30 percent ...
				CPUTimeAdj=$(echo $CPUTime + $CPUTime*0.3 | bc)
			else
				CPUTimeAdj=$(echo $CPUTime + 3600 | bc)
			fi
			echo $CPUTimeAdj
      #CPUOneTenth=$(echo $CPUSec*0.2 | bc)
			#CPUOneTenth=${CPUOneTenth%.*}
			#echo $CPUOneTenth
			#deltaCPU=$(echo $CPUSec - $CPUTime | bc)
			#deltaCPU=${deltaCPU%.*}
			#echo $deltaCPU
			#if [ "$deltaCPU" -lt "$CPUOneTenth" ] && [ "$deltaCPU" -gt 3600 ]; then
				#echo Decreasing Runtime by 30 percent of $deltaCPU
        #CPUTimeAdj=$(echo $CPUTime - $deltaCPU*0.30 | bc)
			#else
				#echo Walltime is good for this simulation
				#echo Leaving unchanged
        #CPUTimeAdj=$(echo $CPUSec | bc)
			#fi
      minutes=`echo "($CPUTimeAdj)/60" | bc`
			hours=`echo "($minutes)/60" | bc`
			minutes=`echo "($minutes)%60" | bc`
			Runtime=${hours}:${minutes}:00
		fi
		echo Your new adjusted WallTime is $Runtime
##############################################################################
### Create new Dir for the next simulation ###################################
##############################################################################
		SaveFileDir=${SaveDirScratch}/FilesRun${SimCounter}
		mkdir -p $SaveFileDir
		mkdir -p $SaveFileDir/MMGMesh
done # end while loop
		rm -rf $SaveFileDir
##############################################################################
### THE END ##################################################################
##############################################################################

