# for dir in 'BFGS' 'GN' 'DogLeg' 'SGD'; do
for dir in 'SGD'; do
  if [ -d "$dir" ]; then
    cd "$dir"
  # Take action if $DIR exists. #
    for func_poly_dir in */; do
        cd "$func_poly_dir"
        amt=$(ls | wc -l)
        echo $amt
        fps=$(echo "scale=2 ; $amt / 5" | bc)
        echo $fps
        ffmpeg -y -framerate $fps -i "%d.png" -filter_complex "palettegen" palette.png && 
        ffmpeg -y -framerate $fps -i "%d.png" -i palette.png -filter_complex "paletteuse" "$(pwd)".gif
        rm palette.png
        cd ..
        # rm -rf "$func_poly_dir"
    done
    cd ..
  fi
  
done