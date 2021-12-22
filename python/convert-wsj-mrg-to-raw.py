#!/usr/bin/env python
# coding: utf-8

# # problem: my PTB WSJ corpus has no .raw files
# 
# ## TODO: convert .mrg files to .raw
# 
# - some PTB WSJ .raw files are available here for comparison: https://github.com/teropa/nlp/tree/master/resources/corpora/treebank/raw

# In[2]:


import glob
import os
import re
import sys

import discoursegraphs as dg


# In[3]:


raw_filepaths = sorted([f for f in dg.find_files('/home/arne/repos/nishida-nakayama/nlp/resources/corpora/treebank/raw')])
mrg_filepaths = sorted([f for f in dg.find_files('/home/arne/corpora/pennTreebank/parsed/mrg/wsj', '*.mrg')])


# In[4]:


def filepath2id(filepath):
    basename = os.path.basename(filepath)
    match = re.search('\d+', basename)
    if match:
        return match.group()
    sys.stderr.write("Can't extract ID from filename: {}".format(basename))

id2mrg = {}
for mf in mrg_filepaths:
    id2mrg[filepath2id(mf)] = mf


# In[5]:


for rf in raw_filepaths:
    doc_id = filepath2id(rf)
    mrg_filepath = id2mrg.get(doc_id)
    if not mrg_filepath:
        print("NO MATCH for", os.path.basename(rf))


# In[80]:


fpath = mrg_filepaths[0]
with open(fpath) as mrgf:
    print fpath
    print(mrgf.read())


# In[94]:


import nltk

mrg_path, mrg_filename = os.path.split(mrg_filepaths[0])
parsed_doc = nltk.corpus.BracketParseCorpusReader(mrg_path, [mrg_filename])
parsed_sents = [s for s in parsed_doc.parsed_sents()]


# In[96]:


parsed_sents[0]


# In[97]:


parsed_sents[1]


# In[101]:


with open(raw_filepaths[0]) as fp:
    print(fp.read())


# In[104]:


s0 = parsed_sents[0]
s0


# In[106]:


' '.join(s0.leaves())


# In[107]:


' '.join(parsed_sents[1].leaves())


# In[121]:


re.sub(' (\,|:|\.|!|\?)', r'\1', '')


# In[155]:


def tree2string(tree):
    """convert an nltk tree (that contains one sentence) into a string.
    
    """
    return ' '.join(tree.leaves())


# # find non-alphanumeric tokens used in WSJ

# In[122]:


len(mrg_filepaths)


# In[123]:


sent_trees = []

for mrg_filepath in mrg_filepaths:
    mrg_path, mrg_filename = os.path.split(mrg_filepath)
    parsed_doc = nltk.corpus.BracketParseCorpusReader(mrg_path, [mrg_filename])
    sent_trees.extend([s for s in parsed_doc.parsed_sents()])


# In[124]:


len(sent_trees)


# In[150]:


from collections import defaultdict
non_word_tokens = defaultdict(list)

for tree in sent_trees:
    for leaf in tree.leaves()[:-1]:  # ignore sentence endings
        if len(leaf) == 1:
            if re.match('\W', leaf):
                non_word_tokens[leaf].append(tree)


# In[158]:


print(non_word_tokens.keys(), )


# In[159]:


tree2string(non_word_tokens['`'][1])


# In[160]:


non_word_tokens['`'][1]


# In[162]:


PTB_BRACKET_ESCAPE = {'(': r'-LRB-',
                       ')': r'-RRB-',
                       '[': r'-LSB-',
                       ']': r'-RSB-',
                       '{': r'-LCB-',
                       '}': r'-RCB-'}
PTB_BRACKET_UNESCAPE = {val:key for (key, val)
                                in PTB_BRACKET_ESCAPE.items()}

def get_nodelabel(node):
    if isinstance(node, nltk.tree.Tree):
        return node.label()
    elif isinstance(node, unicode):
        return node.encode('utf-8')
    else:
        raise ValueError("Unexpected node type: {0}, {1}".format(type(node), node))

def parse_sentencetree(tree):
    for subtree in tree:
        node_label = get_nodelabel(subtree)
        # unescape the node label, if necessary
        node_label = PTB_BRACKET_UNESCAPE.get(node_label, node_label)
        
        if node_label == '-NONE-':  # ignore tokens annotated for traces
            continue

        if isinstance(subtree, unicode):  # subtree is a token
            # we'll have to modify the parent node of a token, since
            # in NLTK Trees, even a leaf node (with its POS tag) is
            # represented as a Tree (an iterator over a single unicode
            # string), e.g. ``Tree('NNS', ['prices'])``
            pos_tag = self.node[parent_node_id]['label']
            token_attrs = {
                'label': node_label, self.ns+':token': node_label,
                self.ns+':pos': pos_tag}
            self.node[parent_node_id].update(token_attrs)
            self.tokens.append(parent_node_id)

        if isinstance(subtree, nltk.tree.Tree):
            self._parse_sentencetree(subtree, parent_node_id=self._node_id)


# In[175]:


ex1 = non_word_tokens['`'][1]
ex1


# In[171]:


ex1.leaf_treeposition(5)


# In[173]:


ex1[(2, 1, 1, 1)]


# In[176]:


def get_parent_treepos(treepos):
    """Given a treeposition, return the treeposition of its parent."""
    if treepos == ():  # this is the root node
        return None
    return treepos[:-1]

def tree2string_notrace(tree):
    mrg_tokens = tree.leaves()
    raw_tokens = []
    for i, tok in enumerate(mrg_tokens):
        leaf_treepos = tree.leaf_treeposition(i)
        parent_treepos = get_parent_treepos(leaf_treepos)
        if parent_treepos:
            parent_node = tree[parent_treepos]
            if get_nodelabel(parent_node) == '-NONE-':
                continue

        raw_tok = PTB_BRACKET_UNESCAPE.get(tok, tok)
    raw_tokens.append(raw_tok)
    return raw_tokens


tree2string_notrace(ex1)

# In[ ]:





# In[ ]:




