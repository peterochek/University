#!/bin/bash

awk '/(WW)/' /var/log/Xorg.0.log | sed 's/(WW)/Warning:/g' > full.log
awk '/(II)/' /var/log/Xorg.0.log | sed 's/(II)/Information:/g' >> full.log
sed 1d full.log | sponge full.log  #temp file also
cat full.log
