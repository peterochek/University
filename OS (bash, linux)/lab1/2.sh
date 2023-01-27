#!/bin/bash

read -r string

while [[ "$string" != "q" ]]
do
  result+=$string
  read -r string
done

echo "$result"
