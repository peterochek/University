# Жёсткие ссылки не могут пересекать фаловые системы или разделы.

# Задание 19
# find ~/test -exec ls -ld {} \ - работает но костыль, можно сделать так: ls -R

max_count=0
max_dir=""

find_max_dir() {
    local current_dir="$1"
    local current_count=$(find "$current_dir" -maxdepth 1 -type f | wc -l)
    
    if [ $current_count -gt $max_count ]; then
        max_count=$current_count
        max_dir="$current_dir"
    fi
    
    for subdir in "$current_dir"/*; do
        if [ -d "$subdir" ]; then
            find_max_dir "$subdir"
        fi
    done
}

top_level_dir="/home/peter/PycharmProjects"
find_max_dir "$top_level_dir"

echo "max dir $max_dir"
echo "files $max_count"
