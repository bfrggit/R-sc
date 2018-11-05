#!/bin/bash

LIST_NUM_SPOTS="5
6
7
8
10
12
15
20
25
30
35
40
45
50
55
60"
POW_EDGES=1.4

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

for num_spots in $LIST_NUM_SPOTS; do
    num_edges_f=$(bc -l <<< "e(${POW_EDGES} * l(${num_spots}))")
    num_edges=$(printf "%.0f" ${num_edges_f})
    for random_seed in `seq 1 1 5`; do
        fn=$(realpath -m "${output_dir}/graph_${num_spots}_${random_seed}.RData")
        echo ${fn}

        prep/prep_graph_random_loop_unif.R \
            -O ${fn} -s ${random_seed} \
            -L ${num_spots} -e ${num_edges} \
            --weight_lower=30 --weight_upper=300
        if [ $? -ne 0 ]; then exit $?; fi
    done
done
