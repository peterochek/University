max_count=0
max_dir=""

stack=("/home/peter/PycharmProjects")

while [ ${#stack[@]} -gt 0 ]; do
    current_dir="${stack[-1]}"
    stack=("${stack[@]:0:(${#stack[@]}-1)}")

    current_count=$(find "$current_dir" -maxdepth 1 -type f | wc -l)

    if [ "$current_count" -gt "$max_count" ]; then
        max_count="$current_count"
        max_dir="$current_dir"
    fi

    for subdir in "$current_dir"/*; do
        if [ -d "$subdir" ]; then
            stack+=("$subdir")
        fi
    done
done

echo "max dir $max_dir"
echo "files $max_count"