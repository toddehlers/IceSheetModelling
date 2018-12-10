cat ../Test* >> FinalOutput
awk '!seen[$0]++' FinalOutput > Tmp
