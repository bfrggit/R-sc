#!/bin/bash

PROB=50
CASE_TYPES=0
SUM_INTERVALS=360
MAX_COST_WORKER=14400
RE_NUMBER='^[0-9]+$'

print_usage() {
    echo >&2 "Usage: $0 NUM_NODES SELECTOR CASE_LOC PROJ_ROOT OUTPUT_FILE"
}

if [ $# -ne 6 ]; then
    print_usage
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

num_nodes="$1"
if [[ ! ${num_nodes} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "NUM_NODES must be an integer"
    exit 1
fi

selector="$2"

case_location="$3"
if [[ ! ${case_location} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "CASE_LOC must be an integer"
    exit 1
fi

proj_root=$(realpath "$4")
if [ $? -ne 0 ]; then exit $?; fi
cd ${proj_root}

output_file=$(realpath -m "$5")
if [ $? -ne 0 ]; then exit $?; fi

if type module >/dev/null 2>&1; then
    module load R/3.5.0
fi

rm -f ${output_file}
touch ${output_file}
if [ $? -ne 0 ]; then exit $?; fi

distance_fn=$(realpath "scenarios/graph_paris_full.RData")
if [ $? -ne 0 ]; then exit $?; fi

sensor_fn=$(realpath "scenarios/types_paris.RData")
if [ $? -ne 0 ]; then exit $?; fi

location_fn=$(realpath "prep_l_paris_v_nodes_RData/location_${num_nodes}_${case_location}.RData")
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2

for case_presence in `seq 1 1 5`; do
presence_fn=$(realpath "prep_p_paris_v_nodes_RData/presence_${num_nodes}_${PROB}_${case_presence}.RData")
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
--additional_field="num_nodes, prob, case_types_id, case_location_id, case_presence_id" \
--additional_value="${num_nodes}, ${PROB}, ${CASE_TYPES}, ${case_location}, ${case_presence}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
