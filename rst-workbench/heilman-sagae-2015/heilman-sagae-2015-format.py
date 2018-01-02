
# coding: utf-8

# In[1]:

import json


# In[2]:

heilman_outfpath = 'article.heilman-sagae-2015.output'


# In[24]:

import discoursegraphs as dg


# In[26]:

hsg = dg.read_hs2015(heilman_outfpath)


# In[27]:

dg.info(hsg)


# In[3]:

with open(heilman_outfpath, 'r') as parsed_file:
    heilman_json = json.load(parsed_file)


# In[4]:

heilman_json.keys()


# In[5]:

for i, edu in enumerate(heilman_json['edu_tokens']):
    print i, u' '.join(edu)


# In[6]:

print type(heilman_json['scored_rst_trees'])
print len(heilman_json['scored_rst_trees'])


# In[7]:

rst_tree = heilman_json['scored_rst_trees'][0]


# In[8]:

rst_tree.keys()


# In[9]:

tree = rst_tree['tree']


# In[10]:

tree


# In[11]:

import nltk


# In[12]:

nltk_tree = nltk.ParentedTree.fromstring(tree)


# In[13]:

nltk_tree


# In[14]:

#nltk_tree.productions()


# In[15]:

nltk_tree.label()


# In[16]:

for subtree in nltk_tree:
    print subtree.label()


# In[17]:

# def get_tree_structure(tree, indent=0):
#     if isinstance(tree, nltk.tree.ParentedTree):
#         ret_str = "{0} {1}\n".format(' '*indent, tree.label())
#         for subtree in tree:
#             ret_str += get_tree_structure(subtree, indent=indent+1)

#     else:
#         ret_str = "{0} {1}\n".format(' '*indent, tree)
#     return ret_str


# In[18]:

# print get_tree_structure(nltk_tree)


# In[19]:

nltk_tree[1]


# In[20]:

nucleus_span = nltk_tree[1][0]
nucleus_span


# In[21]:

# nucleus_span[0][0] = 'foo bar'


# In[22]:

nucleus_span


# In[23]:

nltk_tree[1]


# In[28]:

get_ipython().magic(u'load_ext gvmagic')


# In[30]:

get_ipython().magic(u'dotstr dg.print_dot(hsg)')


# In[ ]:



