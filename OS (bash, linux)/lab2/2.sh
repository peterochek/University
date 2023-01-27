#!/bin/bash

#cmd_list=$(ls --ignore='.* '-1a /sbin | tr '\n' ',')#
#ps aux -C $cmd_list

ps aux | grep -e "/sbin" | awk '{print $2}' | sed '$d' > sbin.txt
