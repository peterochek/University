#!/bin/bash
if [[ $PWD == "$HOME" ]]
then 
	echo "$HOME"
	exit 0
else
	echo "Run script from $HOME"
	exit 1
fi
