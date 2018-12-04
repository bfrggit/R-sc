#!/bin/bash

RE_NUMBER='^[0-9]+$'

print_usage() {
    echo >&2 "Usage: $0 NUM_SPOTS PATH_PLANNER PROJ_ROOT OUTPUT_FILE"
}

if [ $# -ne 4 ]; then
    print_usage
    echo >&2
    echo >&2 "Illegal number of parameters"
    exit 1
fi

num_spots="$1"
if [[ ! ${num_spots} =~ ${RE_NUMBER} ]]; then
    print_usage
    echo >&2
    echo >&2 "NUM_SPOTS must be an integer"
    exit 1
fi

path_planner="$2"

proj_root=$(realpath "$3")
if [ $? -ne 0 ]; then exit $?; fi
cd ${proj_root}

output_file=$(realpath -m "$4")
if [ $? -ne 0 ]; then exit $?; fi

if type module >/dev/null 2>&1; then
    module load R/3.5.0
fi

rm -f ${output_file}
touch ${output_file} 
if [ $? -ne 0 ]; then exit $?; fi

tail_lines=2
for case_graph in `seq 1 1 5`; do
distance_fn=$(realpath "t_pp/prep_RData/graph_${num_spots}_${case_graph}.RData")
if [ $? -ne 0 ]; then exit $?; fi

t_pp/t_pp.R \
--distance_file=${distance_fn} \
--path_planner=${path_planner} \
--max_cost_worker=1200 \
--additional_field="case_graph_id" \
--additional_value="${case_graph}" | tail -n ${tail_lines} | tee -a "${output_file}"
if [ $? -ne 0 ]; then exit $?; fi
tail_lines=1

done
