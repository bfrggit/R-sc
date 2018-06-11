#!/bin/bash

for prob in `seq 5 5 100`; do
	prep/prep_nodes_unif.R -K 10 -N 100 -p $prob -n 5
done

