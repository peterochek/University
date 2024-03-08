#!/bin/bash
cat /etc/passwd | awk -F : '{print "user " $1 " has an id " $3 }' > work3.log
passwd -S root | awk '{print $3}' >> work3.log
cat /etc/group | awk -F ":" '{print $1}' >> work3.log
echo "Be careful!" >> /etc/skel/readme.txt

useradd -p $(openssl passwd -crypt 12345678) u1
groupadd g1
usermod -aG g1 u1
id u1 >> work3.log
cat /etc/group >>  work3.log
usermod -aG g1 vrx26
cat /etc/group | grep ^g1: | awk -F ':' '{print $4}' >> work3.log
usermod -s /usr/bin/sh u1

useradd -p $(openssl passwd -crypt 87654321) u2
mkdir /home/test13
cp work3.log /home/test13/work3-1.log
cp work3.log /home/test13/work3-2.log

chown -R u1:u1 /home/test13
usermod -aG u1 u2

chmod 750 /home/test13
chmod 640 /home/test13/*

mkdir /home/test14
chown -R u1:u1 /home/test14
chmod +t /home/test14

cp /bin/nano /home/test14/nano

chown u1:u1 /home/test14/nano
chmod u+s /home/test14/nano

mkdir /home/test15

echo "pass" > /home/test15/secret_file
chmod 444 /home/test15/secret_file
chmod 111 /home/test15/
