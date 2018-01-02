import os

import discoursegraphs as dg
from discoursegraphs.readwrite.rst.dis import RSTLispDocumentGraph
from discoursegraphs.readwrite.rst.heilman_sagae_2015 import read_hs2015


disdg1 = dg.read_dis(os.path.join(dg.DATA_ROOT_DIR, 'rst-example1.dis'))

hsdg = dg.read_hs2015('article.heilman-sagae-2015.output')
