#!/bin/bash

at -f 1.sh now + 1 minutes;
tail -n 0 -f homedir/report
