#!/bin/bash

sudo find /var/log -type f -name "*.log" | xargs sudo wc -l
