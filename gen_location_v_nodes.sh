#!/bin/bash

NUM_SPOTS=60

if [ $# -ne 1 ]; then
    echo >&2 "Usage: $0 OUTPUT_DIR"
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

output_dir=$(realpath -m $1)
if [ $? -ne 0 ]; then exit $?; fi

mkdir -p ${output_dir} 
if [ $? -ne 0 ]; then exit $?; fi

for num_nodes in `seq 5 5 200`; do
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/location_${num_nodes}_${random_seed}.RData")
        echo ${fn}

        prep/prep_location_unif.R \
            -O ${fn} -s ${random_seed} \
            -L ${NUM_SPOTS} -N ${num_nodes}
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
