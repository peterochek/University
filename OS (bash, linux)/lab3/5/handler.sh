#!/bin/bash

operation=+
accumulator=1

(tail -f channel) |
while true; do
	read LINE;
	case $LINE in
		"+")
			operation=$LINE
			;;
		"*")
			operation=$LINE
			;;
		QUIT)
			echo "exit"
			exit 0
			;;
		*)
			if [[ $LINE =~ [0-9]+  ]];
			then
				accumulator=$(( accumulator $operation $LINE ))
			else
				echo $LINE
			fi
			;;

	esac
	echo $accumulator
done
