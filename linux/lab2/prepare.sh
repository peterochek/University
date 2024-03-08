echo -n "x" | dd of=1b_file bs=1 count=1
dd if=/dev/zero of=1mb_file bs=1M count=1
dd if=/dev/zero of=1gb_file bs=1G count=1