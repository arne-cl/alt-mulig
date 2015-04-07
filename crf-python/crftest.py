
import crfutils

from ner import feature_extractor, fields, separator

demo_file = open('demo.in', 'r')
out_file = open('output.txt', 'wb')

crfutils.main(feature_extractor, fields=fields, sep=separator,
              infile=demo_file, outfile=out_file)

