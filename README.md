
# gpx_tools

A collection of tools for gps files. Mainly centered on .gpx files.
I use a lot of them to process data from garmin etrex device.

Things they can do:
- Convert between file types `<ext>_to_<ext>.sh`
    These are for mass conversion of files. They include a workaround for gpsbabel, which can not handle all types of file names (ex. greek utf-8).
- Simplify gpx track and reduce the number of points, with a given tolerance
- Remove characters from a xml file in order to reduce size but not data
- Search for gpx files that pass through a given coordinates box


*Suggestions and improvements are always welcome.*

*I use those regular, but they have their quirks, may broke and maybe superseded by other tools.*
