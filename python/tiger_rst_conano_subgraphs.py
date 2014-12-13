#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Arne Neumann <discoursekernels.programming@arne.cl>

"""
Usage: script.py k tiger.xml rst.rs3 conano.xml output.pickle

parallel execution:
ls syntax/*.xml | parallel ~/repos/discoursekernels/tiger_rst_conano_subgraphs.py 2 {} rst/{/.}.rs3 connectors/{/} /tmp/{/.}.pickle
"""

import os
import sys
import cPickle as pickle
from discoursegraphs.readwrite import TigerDocumentGraph, RSTGraph, ConanoDocumentGraph
from discoursekernels.subgraph_enumeration import enumerate_all_subgraphs_upto_size_k

def generate_merged_graph(tiger_file, rst_file, conano_file):
    tdg = TigerDocumentGraph(tiger_file)
    rdg = RSTGraph(rst_file)
    cdg = ConanoDocumentGraph(conano_file)
    tdg.merge_graphs(rdg)
    tdg.merge_graphs(cdg)
    return tdg

if __name__ == '__main__':
    if len(sys.argv) != 6:
        sys.exit(1, "Usage: {} k tiger.xml rst.rs3 conano.xml output.pickle".format(sys.argv[0]))
    k_str, tiger_file, rst_file, conano_file, output_file = sys.argv[1:]
    merged_graph = generate_merged_graph(tiger_file, rst_file, conano_file)
    subgraphs = enumerate_all_subgraphs_upto_size_k(merged_graph, int(k_str))
    with open(output_file, 'wb') as pickle_file:
        pickle.dump(subgraphs, pickle_file)
