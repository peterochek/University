#!/bin/bash

awk -f 5.awk -F ":" proc_info.txt | sponge proc_info.txt

