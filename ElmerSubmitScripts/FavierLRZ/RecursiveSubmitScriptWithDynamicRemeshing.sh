#!/bin/bash
###############################################################################
shopt -s expand_aliases # this is necessary to ensure that aliases are expanded
#in the jobscript (In my case this is the listjob alias
source $HOME/.bashrc # load all your aliases. In my case they are in .bashrc
###############################################################################
#### MAKEFILE VARIABLES #######################################################
###############################################################################
#SINPUT
ProcessorNumber=84
Procx=7
Procy=12
MeshBaseName=Mesh
MakeFileName=Makefile
MasterMakeFileName=MASTERMAKEFILE_Favier
MasterMakeFileNameMMG=MASTERMAKEFILE_MMG
MasterSifFileNameInit=MASTER_FavierSmallDomInit.sif
MasterSifFileNameForw=MASTER_FavierSmallDomRestart.sif
###############################################################################
#### END MAKEFILE VARIABLES ###################################################
###############################################################################
###############################################################################
#### SIFFILE VARIABLES ########################################################
###############################################################################
SifFileBaseNameInit=FavierSmallDomInit
SifFileBaseNameForw=FavierSmallDomRestart
MasterSifFileNameMMG=MASTER_MMGFavier.sif
SifFileBaseNameMMG=FavierMMG2
OutputFileBaseInit=IceRiseMMGInit3
OutputFileBaseForw=IceRiseMMGForw3
VertMeshLevels=10 # number of vertical extrusion layers in siffile
InitTime=850 # makes sure that any previous relaxation simulations are deducted
#from perturbation simulation. This is obsolote if timer in Elmer is set to zeros
#at the start of perturbation simulation
TotalSimLength=1000 # in years
IndSimLength=100 # in years
TimeStepSize=0.5 # in years
WriteOutput=10 # in number of timesteps aka TimeStepSize * WriteOutput gives
SaveDirScratch=$SCRATCH/${PWD##*/} # takes name of current Dir and save
TotalSimLength=$(($TotalSimLength+$InitTime))
BedBaseName=bedrock
SurfaceBaseName=zs
BottomBaseName=zb
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
Runtime=09:32:00 #format has to be HH:MM:SS
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
rm FinalOutput*
mkdir -p ${SaveDirScratch}
#cp DEM/Bedrock.xyz DEM/bedrock1
#cp DEM/ZsRise.xyz DEM/zs1
#cp DEM/ZbRise.xyz DEM/zb1


while [ "$YearCount" -lt "$TotalSimLength" ]
do
#for i in `seq 1 1`
#do
				SimCounter=$(($SimCounter+1)) # increment counter
				echo This is $SimCounter Simulation
				mkdir -p ${SaveDirScratch}/FilesRun$SimCounter/
				SaveFileDir=${SaveDirScratch}/FilesRun${SimCounter}
				cd Remeshing
				cp $MasterSifFileNameMMG ${SifFileBaseNameMMG}${SimCounter}.sif
				#sed -i "s|BEDNAME|${BedBaseName}${i}|g" $SifFileBaseNameMMGS$i.sif
				sed -i "s|BEDNAME|${BedBaseName}${SimCounter}|g" $SifFileBaseNameMMG$SimCounter.sif
				sed -i "s|ICETOPNAME|${SurfaceBaseName}${SimCounter}|g" \
				$SifFileBaseNameMMG$SimCounter.sif
				sed -i "s|ICEBOTTOMNAME|${BottomBaseName}${SimCounter}|g" \
				$SifFileBaseNameMMG$SimCounter.sif
				cp $MasterMakeFileNameMMG $MakeFileName
				sed -i "s|SIFFILEMMG|${SifFileBaseNameMMG}${SimCounter}|g" $MakeFileName
				make ini
				ElmerSolver
				mkdir -p ${SaveDirScratch}/FilesRun$SimCounter/Mesh$SimCounter
				cp -r square_iso_N4/mesh* \
				${SaveDirScratch}/FilesRun$SimCounter/Mesh$SimCounter
				cd ..
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
				cp $MasterSifFileNameInit ${SifFileBaseNameInit}${SimCounter}.sif
				sed -i "s|MESHNAME|${MeshBaseName}${SimCounter}|g" \
				$SifFileBaseNameInit$SimCounter.sif
				sed -i "s|MESHPATH|${SaveFileDir}|g" $SifFileBaseNameInit$SimCounter.sif
				sed -i "s|OUTPUTFILEINIT|${OutputFileBaseInit}${SimCounter}|g" \
				$SifFileBaseNameInit$SimCounter.sif
				sed -i "s|BEDNAME|${BedBaseName}${SimCounter}|g" $SifFileBaseNameInit$SimCounter.sif
				sed -i "s|ICETOPNAME|${SurfaceBaseName}${SimCounter}|g" \
				$SifFileBaseNameInit$SimCounter.sif
				sed -i "s|ICEBOTTOMNAME|${BottomBaseName}${SimCounter}|g" \
				$SifFileBaseNameInit$SimCounter.sif
				sed -i "s/MESHLEV/$VertMeshLevels/g" $SifFileBaseNameInit$SimCounter.sif

				cp $MasterSifFileNameForw ${SifFileBaseNameForw}${SimCounter}.sif
				sed -i "s|MESHNAME|${MeshBaseName}${SimCounter}|g" \
				$SifFileBaseNameForw$SimCounter.sif
				sed -i "s|MESHPATH|${SaveFileDir}|g" $SifFileBaseNameForw$SimCounter.sif
				sed -i "s|OUTPUTFILEINIT|${OutputFileBaseInit}${SimCounter}|g" \
				$SifFileBaseNameForw$SimCounter.sif
				sed -i "s|OUTPUTFILEFORW|${OutputFileBaseForw}${SimCounter}|g" \
				$SifFileBaseNameForw$SimCounter.sif
				sed -i "s/MESHLEV/$VertMeshLevels/g" $SifFileBaseNameForw$SimCounter.sif
				sed -i "s/INDSIMLEN/$IndSimLength/g" $SifFileBaseNameForw$SimCounter.sif
				sed -i "s/WRITEFILESTEPS/$WriteOutput/g" $SifFileBaseNameForw$SimCounter.sif
				sed -i "s/STEPSIZE/$TimeStepSize/g" $SifFileBaseNameForw$SimCounter.sif
				sed -i "s/RESSTART/$InitTime/g" $SifFileBaseNameForw$SimCounter.sif

				cp $MasterMakeFileName $MakeFileName
				sed -i "s|MESHNAME|${MeshBaseName}${SimCounter}|g" $MakeFileName
				sed -i "s|MESHPATH|${SaveFileDir}|g" $MakeFileName
				sed -i "s|SIFFILEINIT|${SifFileBaseNameInit}${SimCounter}|g" $MakeFileName
				sed -i "s|SIFFILEFORW|${SifFileBaseNameForw}${SimCounter}|g" $MakeFileName
				sed -i "s/TOTPROCS/$ProcessorNumber/g" $MakeFileName
				sed -i "s/PROCX/$Procx/g" $MakeFileName
				sed -i "s/PROCY/$Procy/g" $MakeFileName
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
				#SimLength=$(cat SLURM_job*$JOBID*.out | grep Time: | awk '{ print $NF }' | tail -1)
				InitTime=$( bc <<< "($InitTime+50)")
        #InitTime=$( bc <<< "($InitTime+$SimLength)")
				#counter=$i
				#((counter++))
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
		CPUTimePart1=$(cat SLURM_job*$JOBID*.out | grep 'SOLVER TOTAL' | head -1 | awk '{print $NF}')
		CPUTimePart2=$(cat SLURM_job*$JOBID*.out | grep 'SOLVER TOTAL' | tail -1 | awk '{print $NF}')
		CPUTime=$( bc <<< "($CPUTimePart1+$CPUTimePart2)")
		echo $CPUTimeFavierSmallDomRestart2.sif
		#if [ -n "${CPUTime+set}" ]; then
		if [ -z "$CPUTime" ]; then
		  echo Darn the job did not finish ....
			echo Increasing walltime for next job
			CPUSec=$(echo $Runtime | awk -F':' '{print $1 * 60 * 60 + $2 * 60 + $3}')
			echo $CPUSec
			CPUTimeAdj=$(echo $CPUSec + $CPUSec*0.2 | bc)
			echo $CPUTimeAdj
			minutes=`echo "($CPUTimeAdj)/60" | bc`
			hours=`echo "($minutes)/60" | bc`
			minutes=`echo "($minutes)%60" | bc`
			Runtime=${hours}:${minutes}:00
		else
		  echo Yay job finished within specified walltime!
			echo Calculating walltime of next job ...
      #CPUSec=$(echo $Runtime | awk -F':' '{print $1 * 60 * 60 + $2 * 60 + $3}')
			echo $CPUTime
			CPUBuffer=$(echo $CPUTime*0.3 | bc)
			echo $CPUBuffer
			if (( $(echo "$CPUBuffer > 7200" | bc -l) )); then
        echo CPUBuffer larger than 45 minutes ...
				echo Increasing Walltime by 30 percent ...
				CPUTimeAdj=$(echo $CPUTime + $CPUTime*0.3 | bc)
			else
				CPUTimeAdj=$(echo $CPUTime + 7200 | bc)
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

				./ProcessSaveLine.sh $SimCounter $SaveFileDir
done
