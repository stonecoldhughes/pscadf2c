import argparse
import re

# open the generated c file. Find and replace the types.

# find the inlude f2c.h line, delete it and all the comments before it
# find and replace occurences of the renamed types

# do this in-place - replace the file

parser = argparse.ArgumentParser( description = 'post-process the converted C file' )

parser.add_argument( '-f', '--file', \
                     help = 'name of the file to overwrite', \
                     required = True )

args = parser.parse_args()

string = ''

# load contents of the C file into a string
with open( args.file, 'r' ) as contents:

  string = contents.read()

# delete everything up to and including this string
trash_string = '#include "f2c.h"'

index = string.find( trash_string )

index += len( trash_string )

string = string[ index : ]

# replace misnamed types
string = string.replace( 'doublereal', 'double' )

string = string.replace( 'integer', 'int' )

# overwrite the original C file:
with open( args.file, 'w' ) as output:

  output.write( string )
