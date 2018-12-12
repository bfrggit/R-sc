#!/bin/bash

NUM_TYPES=10
NUM_NODES=100
NUM_EDGES=300
CASE_GRAPH=4
RE_NUMBER='^[0-9]+$'

print_usage() {
    echo >&2 "Usage: $0 PROB SELECTOR CASE_TYPES CASE_LOC PROJ_ROOT OUTPUT_FILE"
}

if [ $# -ne 6 ]; then
    print_usage
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

prob="$1"
if [[ ! ${prob} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "PROB must be an integer"
    exit 1
fi

selector="$2"

case_types="$3"
if [[ ! ${case_types} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "CASE_TYPES must be an integer"
    exit 1
fi

case_location="$4"
if [[ ! ${case_location} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "CASE_LOC must be an integer"
    exit 1
fi

proj_root=$(realpath "$5")
if [ $? -ne 0 ]; then exit $?; fi
cd ${proj_root}

output_file=$(realpath -m "$6")
if [ $? -ne 0 ]; then exit $?; fi

if type module >/dev/null 2>&1; then
    module load R/3.5.0
fi

rm -f ${output_file}
touch ${output_file} 
if [ $? -ne 0 ]; then exit $?; fi

distance_fn=$(realpath "prep_graph_v_edges_RData/graph_${NUM_EDGES}_${CASE_GRAPH}.RData")
if [ $? -ne 0 ]; then exit $?; fi

sensor_fn=$(realpath "prep_types_RData/types_${NUM_TYPES}_${case_types}.RData")
if [ $? -ne 0 ]; then exit $?; fi

location_fn=$(realpath "prep_location_v_nodes_RData/location_${NUM_NODES}_${case_location}.RData")
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2

for case_presence in `seq 1 1 5`; do
presence_fn=$(realpath "prep_presence_v_prob_RData/presence_${NUM_NODES}_${prob}_${case_presence}.RData")
if [ $? -ne 0 ]; then exit $?; fi

simu/simu.R \
--paranoid \
-W 100 \
-x 1e+4 \
-y 1 \
-z 5 \
-w 0 \
--sensor_file=${sensor_fn} \
--location_file=${location_fn} \
--presence_file=${presence_fn} \
--distance_file=${distance_fn} \
--selector=${selector} \
--path_planner=combined_1 \
--max_cost_worker=3600 \
--additional_field="num_nodes, prob, num_edges, case_types_id, case_location_id, case_presence_id, case_graph_id" \
--additional_value="${NUM_NODES}, ${prob}, ${NUM_EDGES}, ${case_types}, ${case_location}, ${case_presence}, ${CASE_GRAPH}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
