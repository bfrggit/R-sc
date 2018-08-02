#!/bin/bash

NUM_TYPES=10
NUM_NODES=100

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

for prob in `seq 5 5 100`; do
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/presence_${NUM_NODES}_${prob}_${random_seed}.RData")
        echo ${fn}

        prep/prep_presence_unif.R \
            -O ${fn} -s ${random_seed} \
            -K ${NUM_TYPES} -N ${NUM_NODES} -p ${prob}
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
