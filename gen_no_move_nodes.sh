#!/bin/bash

for num_nodes in `seq 5 5 200`; do
	prep/prep_nodes_unif.R -K 10 -N $num_nodes -p 50 -n 5
done

