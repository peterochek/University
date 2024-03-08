#!/bin/bash

rm -f work3.log
rm -f /etc/skel/readme.txt
rm -rf /home/test1*
gpasswd -d vrx26 g1
groupdel g1
userdel u2
userdel u1
rm -rf /home/u1 /home/u2
rm -f /var/spool/mail/u1 /var/spool/mail/u2
