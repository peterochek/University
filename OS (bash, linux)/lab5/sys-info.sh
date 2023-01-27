#!/bin/bash

MEM_TOTAL=$(< /proc/meminfo grep -i "MemTotal" | echo "$(awk '{print $2}')" "/ 1024" | bc)" mB"
SWAP_TOTAL=$(< /proc/meminfo grep -i "SwapTotal" | echo "$(awk '{print $2}')" "/ 1024" | bc)" mB"
PAGE_SIZE=$(getconf PAGE_SIZE)" B"
MEM_AVAILABLE=$(< /proc/meminfo grep -i "MemAvailable" | echo "$(awk '{print $2}')" "/ 1024" | bc)" mB"
SWAP_FREE=$(< /proc/meminfo grep -i "SwapFree" | echo "$(awk '{print $2}')" "/ 1024" | bc)" mB"

echo MEM_TOTAL = "$MEM_TOTAL"
echo SWAP_TOTAL = "$SWAP_TOTAL"
echo PAGE_SIZE = "$PAGE_SIZE"
echo MEM_AVAILABLE = "$MEM_AVAILABLE"
echo SWAP_FREE = "$SWAP_FREE"
