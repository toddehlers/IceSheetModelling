#!/bin/bash

cat $2/li*dat >> FinalOutput$1.txt
#rm *dat*
#awk '{ if ($3==5) print $5, $6, $(NF)}' FinalOutput.txt > groundedmask
awk '{ if ($3==5) print $5, $6, $(NF)}' FinalOutput$1.txt > DEM/bedrock$(($1+1))
awk '{ if ($3==5) print $5, $6, $(NF-1)}' FinalOutput$1.txt > DEM/zb$(($1+1))
awk '{ if ($3==6) print $5, $6, $(NF-2)}' FinalOutput$1.txt > DEM/zs$(($1+1))
