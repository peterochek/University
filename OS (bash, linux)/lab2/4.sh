#!/bin/bash

procs=$(ls /proc -1 | grep '[0-9]' | awk '{print "/proc/" $1}')
# all proc-numeric instances

> proc_info.txt
# clear file

for proc in $procs
do
	status=$proc$"/status"
	sched=$proc$"/sched"
	pid=$(grep "Pid:" "$status" --max-count=1 | awk '{print $2}') 
	ppid=$(grep "PPid:" "$status" | awk '{print $2}')
	sum_exec_runtime=$(grep "se.sum_exec_runtime" "$sched" --max-count=1 | awk '{print $3}')
	nr_switches=$(grep "nr_switches" "$sched" --max-count=1 | awk '{print $3}')
	art=$(echo "$sum_exec_runtime" "/" "$nr_switches" | bc -l | awk '{printf "%.3f", $1}')
	if [[ "$art" == "" ]];
       	then
		continue
	fi
	echo "$pid" : "$ppid"  : "$art" >> proc_info.txt
done

sort -n -t ':' -k2 proc_info.txt | sponge proc_info.txt
