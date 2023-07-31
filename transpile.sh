
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


python /convert_fixed_format.py \
    --name $function_name\
    --source $source_file \
    --destination $destination_file \
    --vars $variables_file ;


# invoke f2c using stdout and stdin to get around the stringent file naming convention
f2c -c -u -a -r8 < $destination_file > /trash.txt;

# post-process the resulting C-file also in-place
python /modify_types.py --file /trash.txt ;

mv /trash.txt $destination_file
