#!/bin/bash

extracted=$(< top.log awk 'NR % 10 == 1' | cut -c 28-)

# echo "$extracted"

free_used_buff_cache=$(echo "$extracted" | sed -e "s/free//" -e "s/used//" -e "s/buff\/cache//" \
                                            -e 's/,//2' -e 's/,//3' | tr , .)

# echo "$free_used_buff_cache"

# free_used_buff_cache=$(echo "$extracted" | awk -F  ',' '{print $3, $5, $7}')

echo "$free_used_buff_cache" > free_used_buff_cache.log