#!/bin/bash

while true; do
    echo "
    1 - nano
    2 - vi
    3 - lynx
    4 - exit
"

    read -r arg

    case "$arg" in
        1 ) nano ;;
        2 ) vi ;;
        3 ) lynx ;;
        4 ) exit 0 ;;
        * ) echo "Try again!"
    esac
done