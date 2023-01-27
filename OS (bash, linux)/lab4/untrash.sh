#!/bin/bash

data=$(tac < ~/.trash.log)
# echo "$data"

num=$(wc -l < ~/.trash.log)
# echo "$num"

while read -r line <&3;
do
	new_num=$(echo "$num" - 1 | bc)
	id=$(echo "$line" | cut -d '|' -f 1)
	path=$(echo "$line" | cut -d '|' -f 2-)  #all after first delimeter

	trash_file_exists=$(test -e ~/.trash/"$id" && echo "1")

	echo "id: $id   path: $path"

	if [[ $trash_file_exists != 1 ]]; then
		continue;
	fi
	if [[ $path =~ .*$1$ ]]; then
		echo "restore this file? $line"
		read -r ans
		if [[ $ans == y || $ans == yes ]]; then
			directory=$( ( (echo "$path" | rev | cut -d"/" -f 2- | rev) && test -d) || ( $HOME ) )  
			if [[ $directory == ~/ ]]; then
				echo directory not found, using ~/
			fi
			file_name=$1
			while :
			do	
				status=$(ln ~/.trash/"$id" "$file_name" && echo "1")
				if [[ $status == 1 ]]; then
					rm ~/.trash/"$id"
					break;
				else
					echo this file already exist, write new name for this file:
					read -r file_name			
				fi
			done
		fi
	fi
	num=$new_num
done 3<~/.trash.log 
