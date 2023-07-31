
# assumes f2c conda environment is activated

while getopts ":n:s:d:v:" options; do       

  case "${options}" in                   

  n)                                  

  function_name=${OPTARG}  
  ;;

  s)                      

  source_file=${OPTARG}  
  ;;

  d)                    

  destination_file=${OPTARG}           
  ;;

  v)                                  

  variables_file=${OPTARG}           
  ;;

  :)                                

  echo "Error: -${OPTARG} requires an argument."
  exit 1
  ;;

  *)                               

  echo "Error - unrecognized option"
  exit 1
  ;;

  esac

done

docker run \
-it --name pscadf2c_container \
--rm \
-v $(pwd):/portal \
pscadf2c \
-n  $function_name \
-s  $source_file \
-d  $destination_file \
-v  $variables_file








