#!/bin/bash

sort -nt ':' -k3 /etc/passwd | awk -F ':' '{print $1, $3}'
# numeric (-t separator) reverse key  (-F separator)