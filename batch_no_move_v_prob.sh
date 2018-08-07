#!/bin/bash

NUM_TYPES=10
NUM_NODES=100

if [ $# -ne 2 ]; then
    echo >&2 "Usage: $0 PROJ_ROOT OUTPUT_FILE"
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

proj_root=$(realpath "$1")
if [ $? -ne 0 ]; then exit $?; fi
cd ${proj_root}

output_file=$(realpath -m "$2")
if [ $? -ne 0 ]; then exit $?; fi

if type module >/dev/null 2>&1; then
    module load R/3.5.0
fi

rm -f ${output_file}
touch ${output_file} 
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2
for prob in `seq 5 5 100`; do

for case_types in `seq 1 1 2`; do
sensor_fn=$(realpath "prep_types_RData/types_${NUM_TYPES}_${case_types}.RData")
if [ $? -ne 0 ]; then exit $?; fi

for case_location in `seq 1 1 2`; do
location_fn=$(realpath "prep_location_v_nodes_RData/location_${NUM_NODES}_${case_location}.RData")
if [ $? -ne 0 ]; then exit $?; fi

for case_presence in `seq 1 1 5`; do
presence_fn=$(realpath "prep_presence_v_prob_RData/presence_${NUM_NODES}_${prob}_${case_presence}.RData")
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
--additional_value="${NUM_NODES}, ${prob}, ${case_types}, ${case_location}, ${case_presence}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
done
done
done
done