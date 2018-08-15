# Teensy-Memory-Map
This is a build tool used to generate the map file for Teensy ARM and may be used to other ARM boards with some slight modificaiton.

To get setup after downloading the source code:
This assumes you have Arduino and the Teensy Arduino Add-In
1)	Compile the VB.NET source to the release directory
2)	Navigate to the Arduino hardware tools directory
3)	Re-name the file 'teensy_post_compile.exe' to 'REAL_teensy_post_compile.exe'
4)	Copy the file 'teensy_post_compile.exe' from the release directory and place it in the Arduino-hardware-tools directory
       i.e. 'C:\Program Files (x86)\Arduino\hardware\tools\'
5)	Verify, through the Arduino IDE preferences (File->Preferences), that the 'Show verbose output during compile’ is checked
6)	Test the memory map generation Using the Arduino IDE and note that it displays the memory map in the compile output window
