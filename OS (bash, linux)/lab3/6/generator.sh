#!/bin/bash

while true; do
	read LINE
	case $LINE
		in
		"TERM")
			kill -SIGTERM $(cat pid)
			exit
			;;
		"+")
			kill -USR1 $(cat pid)
			;;
		"*")
			kill -USR2 $(cat pid)
			;;
		"+1")
			kill -SIGPROF $(cat pid)
			;;
		"-1")
			kill -SIGIO $(cat pid)
			;;
		*)
			:
			;;
	esac
done
