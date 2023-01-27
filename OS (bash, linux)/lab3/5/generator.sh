#!/bin/bash


$(mkfifo channel)

while true
do
    read s
    echo "$s" >> channel

    if [[ "$s" == "QUIT" ]]
    then

        rm channel
        exit 0
    fi

    if [[ ! "$s" =~ [0-9]+ && "$s" != "+" && "$s" != "*" ]]
    then
        echo "Stopped by incorrect message"

        rm channel
        exit 1
    fi
done
