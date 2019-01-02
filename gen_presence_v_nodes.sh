#!/bin/bash

PROB=50
RE_NUMBER='^[0-9]+$'

if [ $# -ne 2 ]; then
    echo >&2 "Usage: $0 NUM_TYPES OUTPUT_DIR"
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

num_types="$1"
if [[ ! ${num_types} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "NUM_TYPES must be an integer"
    exit 1
fi

output_dir=$(realpath -m "$2")
if [ $? -ne 0 ]; then exit $?; fi

mkdir -p ${output_dir}
if [ $? -ne 0 ]; then exit $?; fi

for num_nodes in `seq 5 5 200`; do
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/presence_${num_nodes}_${PROB}_${random_seed}.RData")
        echo ${fn}

        prep/prep_presence_sample_per_type.R \
            -O ${fn} -s ${random_seed} \
            -K ${num_types} -N ${num_nodes} -p ${PROB}
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
