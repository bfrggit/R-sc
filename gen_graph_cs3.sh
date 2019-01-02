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

for random_seed in `seq 1 1 10`; do
    fn=$(realpath -m "${output_dir}/graph_${random_seed}.RData")
    echo ${fn}

    prep/prep_graph_cs3_unif.R \
        -O ${fn} -s ${random_seed} \
        -L ${NUM_SPOTS} \
        --weight_multiplier=1
    if [ $? -ne 0 ]; then exit $?; fi
done
