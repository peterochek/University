#!/bin/bash

grep -s --text -Eioh "\b[a-z0-9._-]+@[a-z0-9-]+\.[a-z]{2,4}\b" -R /etc | sort | uniq | tr '\n' ', ' > emails.lst
# (supress) (no bin files) (man)
