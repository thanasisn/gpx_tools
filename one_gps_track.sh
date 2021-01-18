#!/bin/bash

#### Combine multiple gpx track files matching a pattern


folderin="$HOME/LOGs/one_gps/"
pattern=$1
folder=$2

echo "Usage $(basename $0) <find -iname pattern> <find folder> "
echo "pattern=$pattern"
echo "folder=$folder"

find "$folder" -iname "*$pattern*.gpx" | sort

read -r -p "Are you sure? [y/N] " response
response=${response,,}    # tolower
if [[ ! $response =~ ^(yes|y)$ ]]; then
exit
fi

## copy files to working dir
find "$folder" -iname "*$pattern*.gpx" -exec cp {} "$folderin" \;

## sterilize filenames
detox "$folderin"*

comaand=$(find "$folderin" -iname "*$pattern*.gpx" | sort | while read line; do
printf "-"
printf "f %s  " "$line"
done)

# echo $comaand
# echo "$folderin""$pattern""_combined.gpx"

## all tracks in one file
gpsbabel -i gpx $comaand -o gpx -F "$folderin""$pattern""_combined.gpx"

## all tracks to one track
gpsbabel -i gpx -f "$folderin""$pattern""_combined.gpx" -x track,merge -o gpx -F "${folderin}${pattern}.gpx"

echo
echo "Output file: ${folderin}${pattern}.gpx"
echo

exit 0
