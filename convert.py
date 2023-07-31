import argparse
import re
import csv

# Captain! Allow different data types to be consumed here as well
def declare_params( params, size_key ):

  s = '\n'

  for p in params :

    s += six_spaces + 'real ' + p

    if p in size_key:
      
      s += '( ' + size_key[ p ]  +' )'

    s += '\n'

  return s

def space_line( line ):

  return six_spaces + line + '\n'

  # Captain: original space-line code designed to work with f2c without free format mode
  # new code is above 
  '''
  if( len( line ) <= 66 ): return six_spaces + line + '\n'

  s = ''

  s += six_spaces + line[ : 66 ] + '\n'

  i = 2

  while( i * 66 <= len( line ) ):
    
    substr = line[ ( i - 1 ) * 66 : i * 66 ]

    s += carryover + substr + '\n'

    i = i + 1

  substr = line[ ( i - 1 ) * 66 : ]
  
  s += carryover + substr + '\n'

  return s
  '''

def generate_param_list( params ):

  s = ''

  for p in params[ : -1 ] :

    s += p + ', '

  s += params[ -1 ]

  return s

def generate_header( params ):

  s = 'subroutine F ( '

  param_list = generate_param_list( params )


  s += param_list + ' )'

  return s
 
# main():

parser = argparse.ArgumentParser( description = 'Convert FORTRAN code into standard format' )

parser.add_argument( '-f', '--file', \
                     type = argparse.FileType( 'r' ), \
                     help = 'Specify a Fortran source file to convert to C++', \
                     required = True )

parser.add_argument( '-v', '--vars', \
                     type = argparse.FileType( 'r' ), \
                     help = 'Input file containing the sizes of the array variables', \
                     required = True )

args = parser.parse_args()

# declare new empty file string to build
string = ''

six_spaces = ' ' * 6

carryover = ' ' * 5 + '+'

params = set()

# readlines in the file, loop over each line.
for line in args.file:

  line = line.strip()

  # check skip any comment lines
  c = re.match( r'^!', line )

  if not c:

    m = re.match( r'#local', line )

    # initial variable declaration
    if m:

      line = line[ m.end() : ].strip()

      # check for array parameters and put them in a comma separated list,
      # enclosed with parentheses
      dims = re.match( r'(\w+)\s+(\w+)\s+(\d+)\s+(\d+)', line )

      if dims:

        data_type = dims.group( 1 )
        variable_name = dims.group( 2 )
        rows = dims.group( 3 )
        cols = dims.group( 4 )

        line = f'{data_type} {variable_name} ( {rows}, {cols} )'

    # regular code
    else:

      token = re.search( r'(\$)(\w+)', line )

      while token:

        params.add( token.group( 2 ) )

        line = line[ : token.start() ] + line[ token.start() + 1 : ]

        line = line.strip()

        token = re.search( r'(\$)(\w+)', line )

    line = space_line( line )

    string += line

    string += '\n'

  else:

    string += line + '\n'

#for p in params:

# print( p )

reader = csv.reader( args.vars, delimiter = ' ' )

# skip first row by default as it contains column titles
next( reader )

size_key = {}

for row in reader:

  if len(row) == 2:

    size_key[ row[ 0 ] ] = row[ 1 ]

header = generate_header( list(params) )

header = space_line( header )

param_declaration = declare_params( list(params), size_key )

string = header + '\n' + param_declaration + '\n' + string

string = string + '\n' + six_spaces + 'return\n' + six_spaces + 'end'
#string = string + '\n' + 'return\n' + 'end'

print( string )
