
# coding: utf-8

# In[1]:

import os
from operator import itemgetter
import pygraphviz
import networkx as nx

from discoursegraphs.discoursegraph import rename_tokens
from discoursegraphs.readwrite import RSTGraph
from discoursegraphs.readwrite import TigerDocumentGraph
from discoursegraphs.util import natural_sort_key


# DOC_ID = 'maz-19295'
DOC_ID = 'maz-00002'
TIGER_DIR = '~/repos/pcc-annis-merged/maz176/syntax/'
RST_DIR = '~/repos/pcc-annis-merged/maz176/rst/'
MMAX_DIR = '~/repos/pcc-annis-merged/maz176/coreference/'

rst_filepath = os.path.join(os.path.expanduser(RST_DIR), DOC_ID+'.rs3')
tiger_filepath = os.path.join(os.path.expanduser(TIGER_DIR), DOC_ID+'.xml')


def get_bottom_rst_spans(reversed_docgraph):
    """
    returns a list of (rst segment node id, list of token node ids, list of parent segment node ids) tuples.
    """
    token_spans = []
    for nid, nattrs in reversed_docgraph.nodes(data=True):
        if 'rst:segment' in nattrs['layers']:
            token_span = [token_node for (token_node, segment_node) in reversed_docgraph.in_edges(nid)
                          if 'rst:token' in reversed_docgraph.node[token_node]['layers']]
            if token_span:
                parent_segments = [target_node for (source_node, target_node) in reversed_docgraph.out_edges(nid)]
                token_spans.append( (nid, sorted(token_span, key=natural_sort_key), parent_segments) )

    # sometimes, rs3 segments aren't in linear token order, so we'll sort them
    # by the order of their first token, not by the segment number
    span_dict = {span:(tokens, parents) for (span, tokens, parents) in token_spans}
    onset_dict = {tokens[0]:span for (span, tokens, parents) in token_spans}
    onsets = sorted(onset_dict, key=natural_sort_key)

    bottom_rst_spans = []
    for onset in onsets:
        span = onset_dict[onset]
        tokens, parents = span_dict[span]
        bottom_rst_spans.append( (span, tokens, parents) )
    return bottom_rst_spans


def gen_edge(source, target, **attr):
    return u'"{}" -> "{}" [{}];\n'.format(source, target, ', '.join('{}="{}"'.format(k,v) for k,v in attr.items()))
    
def gen_node(name, **attr):
    return u'"{}" [{}];\n'.format(name, u', '.join(u'{}="{}"'.format(k,v) for k,v in attr.items()))

def gen_subgraph(name, subgraphs=None, nodes=None, edges=None,
                 graph_attrs={'rank': 'same', 'newrank': 'true'},
                 node_attrs={},
                 edge_attrs={}):
    return u"""
subgraph {} {{
    graph [{}];
    node [{}];
    edge [{}];
    
    {}    
    {}    
    {}
}}\n\n""".format(name,
                 u', '.join(u'{}="{}"'.format(k,v) for k,v in graph_attrs.items()),
                 u', '.join(u'{}="{}"'.format(k,v) for k,v in node_attrs.items()),
                 u', '.join(u'{}="{}"'.format(k,v) for k,v in edge_attrs.items()),
                 '\t'.join(nodes) if nodes else '',
                 '\t'.join(edges) if edges else '',
                 ''.join(subgraphs) if subgraphs else '')

def gen_cluster(name, subgraphs=None, nodes=None, edges=None,
                graph_attrs={'rank': 'same', 'newrank': 'true'},
                node_attrs={},
                edge_attrs={}):
    return gen_subgraph("cluster_"+name, subgraphs, nodes, edges,
                        graph_attrs, node_attrs, edge_attrs)

def gen_digraph(subgraphs=None, nodes=None, edges=None,
                graph_attrs={'newrank': 'true', 'compound': 'true'},
                node_attrs={'fontname': 'Sans-Serif'},
                edge_attrs={'fontname': 'Sans-Serif'}):
    return u"""
digraph {{
    graph [{}];
    node [{}];
    edge [{}];

   {}   
   {}
   {}
}}
""".format(u', '.join(u'{}="{}"'.format(k,v) for k,v in graph_attrs.items()),
           u', '.join(u'{}="{}"'.format(k,v) for k,v in node_attrs.items()),
           u', '.join(u'{}="{}"'.format(k,v) for k,v in edge_attrs.items()),
           ''.join(subgraphs) if subgraphs else '',
           '\t'.join(nodes) if nodes else '',
           '\t'.join(edges) if edges else '')


def reversedrst2manualdot(nxgraph):
    digraph = nx.DiGraph(nxgraph) # convert multidigraph to digraph
    digraph.tokens = nxgraph.tokens
    
    bottom_rst_spans = get_bottom_rst_spans(digraph)
    bottom_rst_nodes = [segment_node for (segment_node, token_nodes, parent_nodes) 
                        in bottom_rst_spans]

    hierarchy_node_ids = [node for node in digraph.nodes()
                       if node not in digraph.tokens
                       and node not in bottom_rst_nodes]

    # generate hierarchical nodes (non-terminals, non-bottom-rst-segments)
    hierarchy_nodes = []       
    for node_id in hierarchy_node_ids:
        if 'label' in digraph.node[node_id]:
            hierarchy_nodes.append(gen_node(node_id,
                                            label=digraph.node[node_id]['label']))
        else:
            hierarchy_nodes.append(gen_node(node_id))

    # generate hierarchical edges (i.e. edges that don't start from a token node)
    hierarchical_edges = []
    for source, target, edge_attrs in digraph.edges_iter(data=True):
        # don't add edges from tokens or bottom segments
        if source in hierarchy_node_ids:
            if 'label' in digraph.edge[source][target]:
                hierarchical_edges.append(gen_edge(source,
                                                   target,
                                                   label=digraph.edge[source][target]['label']))
            else:
                hierarchical_edges.append(gen_edge(source, target))
            

    # leaving out segment nodes by connecting token nodes directly to
    # the parent nodes of segment nodes
    segment_clusters = []
    token2segment_edges = []
    cluster_precedences = []
    for si, (segment_node_id, token_node_ids, parent_node_ids) in enumerate(bottom_rst_spans):
        #~ print "DEBUG in segment {} (#{}), tokens {}".format(segment_node_id, si, token_node_ids)
        
        # create all token nodes in a bottom rst segment
        token_nodes = []
        for token_node_id in token_node_ids:
            token_nodes.append(gen_node(token_node_id,
                                        label=digraph.node[token_node_id]['label']+"_"+token_node_id))
        
        # generate invisible edges between the tokens of a segment
        token_precedences = []
        for i, token_node_id in enumerate(token_node_ids[:-1]):
            token_precedences.append(gen_edge(token_node_id, token_node_ids[i+1],
                                              style='invis'))
        
        # add all token nodes (and their precedence edges) to a subgraph
        token_subgraph = gen_subgraph(name='tokens_segment{}'.format(segment_node_id),
                                      nodes=token_nodes, edges=token_precedences,
                                      graph_attrs={'rank': 'same'})
        
        # add the subgraph to a bottom rst segment cluster
        cluster_name = 'segment{}'.format(segment_node_id)
        segment_clusters.append(gen_cluster(name=cluster_name, subgraphs=[token_subgraph],
                                            graph_attrs={'rank': 'same', 'style': 'filled',
                                                         'color': 'lightgrey',
                                                         'label': cluster_name},
                                            node_attrs={'style': 'filled',
                                                        'color': 'white'}))
        
        # generate edges from the first token of a segment to all the parent nodes
        # of the segment (with its tail pointing to the cluster)
        for parent_node_id in parent_node_ids:
            token2segment_edges.append(gen_edge(token_node_ids[0],
                                                parent_node_id,
                                                ltail="cluster_"+cluster_name))
        
        # generate precedence relations (i.e. edges between clusters)
        # by adding an edge from the last token of segment i to the
        # first token of segment i+1
        try:
            _next_segment, next_tokens, _next_parents = bottom_rst_spans[si+1]
            #~ print "\tDEBUG next segment {} (#{}), tokens {}".format(_next_segment, si+1, next_tokens)
            #~ print "\tDEBUG add edge: {} --> {}".format(token_node_ids[-1], next_tokens[0])
            cluster_precedences.append(gen_edge(token_node_ids[-1],
                                                next_tokens[0],
                                                style='invis',
                                                constraint='false'))
        except IndexError: # no outgoing edge from last cluster
            pass

    dotgraph = gen_digraph(subgraphs=segment_clusters,
                           nodes=hierarchy_nodes,
                           edges=token2segment_edges+hierarchical_edges+cluster_precedences)
    return dotgraph


if __name__ == '__main__':
    tdg = TigerDocumentGraph(tiger_filepath)
    rdg = RSTGraph(rst_filepath)
    rdg.reverse(copy=False)
    reversed_rdg = rdg # just a new name to clarify things

    rename_tokens(reversed_rdg, tdg) # use Tiger token IDs in the RST graph

    #~ import pudb; pudb.set_trace()
    reversedrst_dot_str = reversedrst2manualdot(reversed_rdg)
    with open("rst-reversed-manual-clusters.dot", 'w') as outfile:
        outfile.write(reversedrst_dot_str.encode('utf8'))

