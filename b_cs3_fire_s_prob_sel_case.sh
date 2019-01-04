#!/bin/bash

NUM_NODES=100
CASE_TYPES=0
SUM_INTERVALS=360
MAX_COST_WORKER=7200
RE_NUMBER='^[0-9]+$'

print_usage() {
    echo >&2 "Usage: $0 PROB SELECTOR CASE_GRAPH CASE_LOC PROJ_ROOT OUTPUT_FILE"
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

case_graph="$3"
if [[ ! ${case_graph} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "CASE_GRAPH must be an integer"
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

distance_fn=$(realpath "prep_graph_cs3_60_RData/graph_${case_graph}.RData")
if [ $? -ne 0 ]; then exit $?; fi

sensor_fn=$(realpath "scenarios/types_cs3_fire.RData")
if [ $? -ne 0 ]; then exit $?; fi

location_fn=$(realpath "prep_location_v_nodes_RData/location_${NUM_NODES}_${case_location}.RData")
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2

for case_presence in `seq 1 1 5`; do
presence_fn=$(realpath "prep_p_cs3_v_prob_RData/presence_${NUM_NODES}_${prob}_${case_presence}.RData")
if [ $? -ne 0 ]; then exit $?; fi

simu/simu.R \
--paranoid \
--sum_intervals=${SUM_INTERVALS} \
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
--max_cost_worker=${MAX_COST_WORKER} \
--additional_field="num_nodes, prob, case_types_id, case_location_id, case_presence_id, case_graph_id" \
--additional_value="${NUM_NODES}, ${prob}, ${CASE_TYPES}, ${case_location}, ${case_presence}, ${case_graph}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
