#!/bin/bash
rm -rf homedir/test;
(mkdir -p homedir/test && (echo "catalog test was created successfully" >> homedir/report ; touch homedir/test/"$(date)"))

ping "www.net_nikogo.ru" || echo $(date) "ping_error" >> homedir/report
