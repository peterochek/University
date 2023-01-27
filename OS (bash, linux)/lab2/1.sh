#!/bin/bash

ps a -Ao user,pid | awk -v user="$1" '{if ($1 == user) print $0}' | wc -l > user_ps.txt
#-v assign var

ps a -Ao pid,command -u "$1" | awk '{print $1 ":" $2}' >> user_ps.txt

#a -> bsd, all, outp format, u - user

#wc -l > wc -l (second cmd) ??
