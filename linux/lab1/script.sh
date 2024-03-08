#!/bin/sh

# Step 1: Create a test directory in the user's home directory
printf "Step 1\n"
mkdir -p ~/test

# %y -> (d - directory, f - file, l - links (file)).
# Step 2: Create a list file with directory and file names from /etc
printf "\nStep 2\n"
sudo find /etc -printf "%y:%p\n" > ~/test/list

# Step 3: Print the number of subdirectories and hidden files in /etc
printf "\nStep 3\n"
subdirs=$(sudo find /etc -type d | wc -l)
hidden_files=$(sudo find /etc -type f -name ".*" | wc -l)
echo "$subdirs" >> ~/test/list
echo "$hidden_files" >> ~/test/list

# Step 4: Create a links directory in the test directory
printf "\nStep 4\n"
mkdir -p ~/test/links

# Step 5: Create a hard link to the list file
printf "\nStep 5\n"
ln ~/test/list ~/test/links/list_hlink

# Step 6: Create a symbolic link to the list file
printf "\nStep 6\n"
ln -s ~/test/list ~/test/links/list_slink

# Step 7: Display the number of hard links
printf "\nStep 7\n"
list_hlink_links=$(stat -c %h ~/test/links/list_hlink)
list_links=$(stat -c %h ~/test/list)
list_slink_links=$(stat -c %h ~/test/links/list_slink)
echo "list_hlink links: $list_hlink_links"
echo "list links: $list_links"
echo "list_slink links: $list_slink_links"

# Step 8: Add the number of lines in the list file to list_hlink
# list and list_hlink point at the same inode -> same file
printf "\nStep 8\n"
line_count=$(wc -l < ~/test/list)
echo "$line_count" >> ~/test/links/list_hlink

# -z -> if empty (or zero length)
# Step 9: Compare list_hlink and list_slink
printf "\nStep 9\n"
diff_result=$(diff ~/test/links/list_hlink ~/test/links/list_slink)
if [ -z "$diff_result" ]; then
    echo "YES"
else
    echo "NO"
fi

# Step 10: Rename the list file to list1
printf "\nStep 10\n"
mv ~/test/list ~/test/list1

# Step 11: Compare list_hlink and list_slink
# Error because link_slink point to empty file???
printf "\nStep 11\n"
if [ -f "~/test/links/list_slink" ]; then
    diff_result=$(diff ~/test/links/list_hlink ~/test/links/list_slink)
    if [ -z "$diff_result" ]; then
        echo "YES"
    else
        echo "NO"
    fi
else 
    echo "list_slink file does not exist"
fi



# Step 12: Create a hard link to the links directory in the user's home directory
printf "\nStep 12\n"
# Impossible (can create cyclic deps in fs)
# https://askubuntu.com/questions/210741/why-are-hard-links-not-allowed-for-directories
ln ~/test/links ~/links

# Step 13: Create list_conf containing .conf files in /etc and subdirectories
printf "\nStep 13\n"
sudo find /etc -type f -name "*.conf" > ~/list_conf

# Step 14: Create list_d containing .d directories in /etc
printf "\nStep 14\n"
sudo find /etc -type d -name "*.d" > ~/list_d

# Step 15: Create list_conf_d by concatenating list_conf and list_d
printf "\nStep 15\n"
cat ~/list_conf ~/list_d > ~/list_conf_d

# Step 16: Create a hidden subdirectory in the test directory
printf "\nStep 16\n"
mkdir -p ~/test/.sub

# Step 17: Copy list_conf_d to the hidden subdirectory
printf "\nStep 17\n"
cp ~/list_conf_d ~/test/.sub

# Step 18: Copy list_conf_d with backup
printf "\nStep 18\n"
cp -b ~/list_conf_d ~/test/.sub

# Step 19: Display the full list of files and subdirectories in the test directory
# find ~/test ?????
printf "\nStep 19\n"
find ~/test -exec ls -ld {} \;

# Step 20: Create a man.txt file with documentation for the man command
printf "\nStep 20\n"
man man > ~/man.txt

# Step 21: Split man.txt into 1 KB files
printf "\nStep 21\n"
split -b 1k ~/man.txt ~/man_part

# Step 22: Create the man.dir directory in the test directory
printf "\nStep 22\n"
mkdir -p ~/test/man.dir

# Step 23: Move the split files to man.dir
printf "\nStep 23\n"
mv ~/man_part* ~/test/man.dir

# Step 24: Assemble the split files back into man.txt
printf "\nStep 24\n"
cat ~/test/man.dir/man_part* > ~/test/man.dir/man.txt

# Step 25: Compare man.txt in the home directory and man.dir
printf "\nStep 25\n"
diff_result=$(diff ~/man.txt ~/test/man.dir/man.txt)
if [ -z "$diff_result" ]; then
    echo "YES"
else
    echo "NO"
fi

# Step 26: Add lines to the beginning and end of man.txt
# - = stdin
printf "\nStep 26\n"
echo "Added lines at the beginning" | cat - ~/man.txt > ~/man_temp && mv ~/man_temp ~/man.txt
echo "Added lines at the end" >> ~/man.txt

# Step 27: Create a patch file
# u = unified
printf "\nStep 27\n"
diff -u ~/test/man.dir/man.txt ~/man.txt > ~/man.patch

# Step 28: Move the patch file to man.dir
printf "\nStep 28\n"
mv ~/man.patch ~/test/man.dir

# Step 29: Apply the patch to man.txt in man.dir
printf "\nStep 29\n"
patch ~/test/man.dir/man.txt ~/test/man.dir/man.patch

# Step 30: Compare man.txt in the home directory and man.dir
printf "\nStep 30\n"
diff_result=$(diff ~/man.txt ~/test/man.dir/man.txt)
if [ -z "$diff_result" ]; then
    echo "YES"
else
    echo "NO"
fi
