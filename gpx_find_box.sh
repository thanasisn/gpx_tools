#!/bin/bash
## created on 2013-07-30

#### search gpx files and based on coordinate box
## apparently this is slow

## This script is superseded by another, but it should work.

# South
latmin=39.93
# North
latmax=40.03

# West
lonmin=23.53
# East
lonmax=23.69



find \
    "$HOME/Documents/Running/Polar" \
    "$HOME/GISdata/GPX" \
    -type f -iname "*.gpx" |\
while read file; do
    echo " >>>  $file"
    lat_line=0
    lon_line=0
    testf="$file"

    ## latitude search
    while read line; do
        if ( [[ $(echo "$line > $latmin" | bc -l) == 1 ]] && [[ $(echo "$line < $latmax" | bc -l) == 1 ]] ); then
            echo "Found lat=  $line"
            lat_line=$line
            echo $testf
            found_lat=1
            break
        else
            #echo "no"
            continue
        fi
    done < <(cat $testf | grep "<trkpt .*>" | grep -oP 'lat=".*?"' | grep -o "[0-9.,]*")

    ## latitude search
    while read line; do
        if ( [[ $(echo "$line > $lonmin" | bc -l) == 1 ]] && [[ $(echo "$line < $lonmax" | bc -l) == 1 ]] ); then
            echo "Found lon=  $line"
            lon_line=$line
            echo $testf
            found_lon=1
            break
        else
            #echo "no"
            continue
        fi
    done < <(cat $testf | grep "<trkpt .*>" | grep -oP 'lon=".*?"' | grep -o "[0-9.,]*")


    if [ "$lat_line" -eq 1 ] || [ "$lon_line" -eq 1 ]; then
        echo $found_lat $lat_line
        echo $found_lon $lon_line
        echo "Found :  $testf"
    fi


done


# #testf="/home/athan/mainnav-tracklogs/13-07-29_18:50/track_13-07-27_06:25.gpx"
# testf=$file
# ## latitude search
# while read line; do
#     if ( [[ $(echo "$line > $latmin" | bc -l) == 1 ]] && [[ $(echo "$line < $latmax" | bc -l) == 1 ]] ); then
#         echo "Found lat=  $line"
#         lat_line=$line
#         echo $testf
#         found_lat=1
#         break
#     else
#         #echo "no"
#         continue
#     fi
# done < <(cat $testf | grep "<trkpt .*>" | grep -oP 'lat=".*?"' | grep -o "[0-9.,]*")
#
# ## latitude search
# while read line; do
#     if ( [[ $(echo "$line > $lonmin" | bc -l) == 1 ]] && [[ $(echo "$line < $lonmax" | bc -l) == 1 ]] ); then
#         echo "Found lon=  $line"
#         lon_line=$line
#         echo $testf
#         found_lon=1
#         break
#     else
#         #echo "no"
#         continue
#     fi
# done < <(cat $testf | grep "<trkpt .*>" | grep -oP 'lon=".*?"' | grep -o "[0-9.,]*")

# cat $testf | grep "<trkpt .*>" | grep -oP 'lon=".*?"' | grep -o "[0-9.,]*"

exit 0
