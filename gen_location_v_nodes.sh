#!/bin/bash

RE_NUMBER='^[0-9]+$'

if [ $# -ne 2 ]; then
    echo >&2 "Usage: $0 NUM_SPOTS OUTPUT_DIR"
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

num_spots="$1"
if [[ ! ${num_spots} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "NUM_SPOTS must be an integer"
    exit 1
fi

output_dir=$(realpath -m "$2")
if [ $? -ne 0 ]; then exit $?; fi

mkdir -p ${output_dir}
if [ $? -ne 0 ]; then exit $?; fi

for num_nodes in `seq 5 5 200`; do
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/location_${num_nodes}_${random_seed}.RData")
        echo ${fn}

        prep/prep_location_unif.R \
            -O ${fn} -s ${random_seed} \
            -L ${num_spots} -N ${num_nodes}
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
