file_blocks=$(debugfs -R "blocks /home/peter/university/3course/linux/lab2/1b_file" /dev/nvme1n1p2)
echo "Blocks of 1B file: $file_blocks"

file_blocks=$(debugfs -R "blocks /home/peter/university/3course/linux/lab2/1mb_file" /dev/nvme1n1p2)
echo "Blocks of 1MB file: $file_blocks"

file_blocks=$(debugfs -R "blocks /home/peter/university/3course/linux/lab2/1gb_file" /dev/nvme1n1p2)
echo "Blocks of 1GB file: $file_blocks"