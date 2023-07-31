# pscadf2c
Transpile PSCAD Fortran files to equivalent C

## Setup

This utility runs multiple steps within a Docker image. The image can be built locally
or pulled from DockerHub.

### Local Image Build

Run this command:

> bash build_pscadf2c.sh

### Pull from Dockerhub

> docker pull FILL_THIS_IN

## Usage

Once the container image is on your system, convert a file:

> bash run_pscadf2c.sh -s source_file -d destination_file -n function_name -v vars_file

source_file:        the Fortran source file
destination_file:   the C file you want to create
function_name:      the converted code will be in a function with this name
vars_file:          this file contains definitions for external variables not defined in the body of the Fortran source file

To see an example, please run:

bash run_pscadf2c.sh -v distribution_grid_pins.csv -s distribution_grid.f -n 
