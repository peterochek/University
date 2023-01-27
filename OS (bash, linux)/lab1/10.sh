#!/bin/bash

man bash | tr -sc '[:alpha:]' '\n' | awk '{if (length($1) >= 4) print $1}' | sort | uniq -c | sort -nr | head -n3 | awk '{print $2}'
# -s (\n\n\n -> \n) (c - inverse)  | words >= 4 | rest (123 value ... | sort by occurences)
