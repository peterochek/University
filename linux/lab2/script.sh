# Prefix all with sudo (so no text noise) !!!

# 1
fdisk /dev/sda1
# Input: n
# Input: p (default)
# Input: 1 (default)
# Input: 2048 (default)
# Input: +300M
# Input: w

# 3
mkfs.ext4 -b 4096 /dev/sda1

# 2
# UUID="...." BLOCK_SIZE="4096" TYPE="ext4"
blkid /dev/sda1 | awk '{print $2}' | cut -c 7- | rev | cut -c 2- | rev > uuid

# 4
dumpe2fs -h /dev/sda1

# 5
tune2fs -c 2 -i 2m /dev/sda1

# 6
mkdir /mnt/newdisk
mount /dev/sda1 /mnt/newdisk/

# 7
ln -s /mnt/newdisk usb

#8
mkdir /mnt/newdisk/tmp

# 9
# 0 0 - dumping / checking (default)
echo "/dev/sda1 /mnt/newdisk ext4 rw,noatime,noexec 0 0" >> /etc/fstab
# sudo ./exe.sh
# sudo: unable to execute ./exe.sh: Permission denied -> noexec works!!

# 10
fdisk /dev/sda
# Input: d (delete partition 1)
# Input: n
# Input: p (default)
# Input: 1 (default)
# Input: 2048 (default)
# Input: +300M
# Input: w

# 11
# -f = force, -n = readonly
umount /mnt/newdisk 
e2fsck -fn /dev/sda1
# e2fsck 1.47.0 (5-Feb-2023)
# Pass 1: Checking inodes, blocks, and sizes
# Pass 2: Checking directory structure
# Pass 3: Checking directory connectivity
# Pass 4: Checking reference counts
# Pass 5: Checking group summary information
# /dev/sda1: 12/89616 files (0.0% non-contiguous), 9831/89600 blocks

# 12
fdisk /dev/sda
# Input: d (delete partition 1)
# Input: n
# Input: p (default)
# Input: 2 (default)
# Input: 2048 (default)
# Input: +12M
# Input: w

#-O [^]feature[,...] Set or clear the indicated filesystem features (options) in the filesystem. 
# Filesystem features prefixed with a caret character ('^') will be cleared in the filesystem's superblock
tune2fs -O ^has_journal /dev/sda1

# mke2fs -O journal_dev (external-journal)
mke2fs -O journal_dev /dev/sda2

# -J override ext3
tune2fs -j -J device=/dev/sda2 /dev/sda1

mount /dev/sda1 /mnt/newdisk/

# 13
fdisk /dev/sda
# Input: d (delete partition 1)
# Input: n
# Input: p (default)
# Input: 2 -> 3 (default)
# Input: 2048 (default)
# Input: +100M
# Input: w x2

# 14
mkdir /mnt/supernewdisk

pvcreate /dev/sda3 /dev/sda4
vgcreate partgroup /dev/sda3 /dev/sda4

lvcreate -l 100%FREE -n volume partgroup
mkfs.ext4 -b 4096 /dev/partgroup/volume
mount /dev/partgroup/volume /mnt/supernewdisk

# 15
mkdir /mnt/share
mount.nfs4 192.168.0.107:/home/peter/university/3course/linux/lab2/share /mnt/share

# 16
echo "192.168.0.107:/home/peter/university/3course/linux/lab2/share /mnt/share nfs noexec 0 0" >> /etc/fstab