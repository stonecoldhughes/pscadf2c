# pscadf2c
Transpile PSCAD Fortran files to equivalent C

## Setup

This utility runs multiple steps within a Docker image. The image can be built locally
or pulled from DockerHub.

### Platforms

`Ubuntu 20.04` or newer with `bash` terminal.

Since PSCAD runs natively in Windows, this may present an obstacle for Windows users.
Luckily, Windows provides native Linux OS support through the WSL2 feature.
Users running on Windows should create an Ubuntu instance in WSL2 and use the utility from there.

Detailed steps for configuring WSL2 in Windows for novice users are provided in the Wiki.

### Local Image Build

Run this command:

`bash build_pscadf2c.sh`

### Pull from Dockerhub

`docker pull FILL_THIS_IN`

## Usage

Once the container image is on your system, convert a file:

`bash run_pscadf2c.sh -s <source_file> -d <destination_file> -n <function_name> -v <vars_file>`

> **source_file**:        *the Fortran source file*

> **destination_file**:   *the C file you want to create*

> **function_name**:      *the converted code will be in a function with this name*

> **vars_file**:          *this file contains definitions for external variables not defined in the body of the Fortran source file*

To see an example, please run:

`bash run_pscadf2c.sh -v distribution_grid_pins.csv -s distribution_grid.f -n pick_a_function_name -d generate_this_file.c`

This will create a file called `generate_this_file.c` containing a function called `pick_a_function_name__` with appropriate function arguments
automatically populated.

To see this example in action, please refer to the CI runs for this project. Click on "Actions" and then "Docker CI action" to see "build" and "run" stages.
Clicking these will demonstrate these steps.
