#!/bin/bash

## Simpify track within an error limit.
## This will reduce the number of track points and the overall size of the file.

number=$1
file=$2

filename="${file%.*}"

echo "file  $file"
echo "error $number k"


gpsbabel -i gpx -f - -x simplify,crosstrack,error="$number"k -o gpx -F -   <"$file"  > "${filename}_E${number}.gpx"



# count option
#
# Maximum number of points in route.
#
# This option specifies the maximum number of points which may appear in the simplified route. For example, if you specify "count=50", all resulting routes will contain 50 points or fewer.
#
# You must specify either this option or the error option.
# error option
#
# Maximum error.
#
# This option specifies the maximum allowable error that may be introduced by removing a single point. Used with the length and crosstrack methods, the value of this option is a distance, specified in miles by default. You may also specify the distance in kilometers by adding a 'k' to the end of the number. For the relative method it is a dimensionless quantity.
#
# How the error is determined depends on whether the length, crosstrack, or relative method is used. If you are using the length method, the error is the change in the length of the route introduced by removing a point. If you are using the crosstrack method, the error is the distance from the point to the line that results if that point is removed. If you are using the relative method, the error is the ratio between the crosstrack error and the horizontal accuracy (derived from HDOP data).
# crosstrack option
#
# Use cross-track error (default).
#
# This option instructs GPSBabel to remove points that have the smallest overall effect on the overall shape of the route. Using this method, the first point to be removed will be the one that is closest to a line drawn between the two points adjacent to it.
#
# If neither this option nor the length option is specified, this is the default.
# length option
#
# Use arclength error.
#
# This option instructs GPSBabel to simplify by removing points that cause the smallest change in the overall length of the route first.
# relative option
#
# Use relative error.
#
# Similar to the crosstrack method, but the error introduced by removing a point is set into relation to its associated horizontal accuracy, determined as 6m * HDOP. If there is timestamp information, the distance to the interpolated point between the two neighboring points is used instead of the distance to their connecting line.
#
# The effect of the relative method is similar to a combination of the crosstrack method with the discard filter: points are removed preserving the overall shape of the route (track), but preferably those that are unreliable.
#
