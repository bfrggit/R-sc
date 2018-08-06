#!/bin/bash

NUM_TYPES=10
PROB=50

if [ $# -ne 1 ]; then
    echo >&2 "Usage: $0 OUTPUT_FILE"
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

output_file=$(realpath -m "$1")
if [ $? -ne 0 ]; then exit $?; fi

if type module >/dev/null 2>&1; then
    module load R
fi

cd $(dirname $(realpath "$0"))

rm -f ${output_file}
touch ${output_file} 
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2
for num_nodes in `seq 5 5 200`; do

for case_types in `seq 1 1 2`; do
sensor_fn=$(realpath "prep_types_RData/types_${NUM_TYPES}_${case_types}.RData")
if [ $? -ne 0 ]; then exit $?; fi

for case_location in `seq 1 1 2`; do
location_fn=$(realpath "prep_location_v_nodes_RData/location_${num_nodes}_${case_location}.RData")
if [ $? -ne 0 ]; then exit $?; fi

for case_presence in `seq 1 1 5`; do
presence_fn=$(realpath "prep_presence_v_nodes_RData/presence_${num_nodes}_50_${case_presence}.RData")
if [ $? -ne 0 ]; then exit $?; fi

for selector in "minimal" "all" "nodal" "local"; do
simu/simu_no_move.R \
--paranoid \
-W 100 \
-x 1e+5 \
-y 1 \
--sensor_file=${sensor_fn} \
--location_file=${location_fn} \
--presence_file=${presence_fn} \
--selector=${selector} \
--additional_field="num_nodes, prob, case_types_id, case_location_id, case_presence_id" \
--additional_value="${num_nodes}, ${PROB}, ${case_types}, ${case_location}, ${case_presence}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
done
done
done
done
