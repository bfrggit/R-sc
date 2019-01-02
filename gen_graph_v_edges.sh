#!/bin/bash

NUM_SPOTS=60
LIST_NUM_EDGES="60
180
300
600
1200
1800
2400
3540"

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

for num_edges in $LIST_NUM_EDGES; do
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/graph_${num_edges}_${random_seed}.RData")
        echo ${fn}

        prep/prep_graph_random_loop_unif.R \
            -O ${fn} -s ${random_seed} \
            -L ${NUM_SPOTS} -e ${num_edges} \
            --weight_lower=30 --weight_upper=300
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
