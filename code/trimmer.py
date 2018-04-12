import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

dubious_webs=['WEB218.web','WEB220.web','WEB225.web','WEB236.web','WEB238.web','WEB240.web','WEB244.web','WEB246.web','WEB251.web','WEB256.web','WEB307.web','WEB33.web','WEB345.web','WEB350.web','WEB351.web','WEB357.web','WEB359.web','WEB41.web','WEB80.web','WEB214.web','WEB215.web','WEB216.web','WEB217.web','WEB219.web','WEB221.web','WEB222.web','WEB223.web','WEB224.web','WEB291.web','WEB292.web','WEB342.web','WEB114.web','WEB117.web','WEB119.web','WEB120.web','WEB153.web','WEB154.web','WEB18.web','WEB180.web','WEB181.web','WEB182.web','WEB183.web','WEB202.web','WEB213.web','WEB213.web','WEB22.web','WEB23.web','WEB23.web','WEB24.web','WEB249.web','WEB250.web','WEB252.web','WEB253.web','WEB254.web','WEB254.web','WEB255.web','WEB255.web','WEB257.web','WEB257.web','WEB258.web','WEB258.web','WEB259.web','WEB259.web','WEB26.web','WEB260.web','WEB260.web','WEB261.web','WEB261.web','WEB262.web','WEB262.web','WEB265.web','WEB265.web','WEB266.web','WEB267.web','WEB268.web','WEB269.web','WEB269.web','WEB288.web','WEB289.web','WEB290.web','WEB290.web','WEB293.web','WEB293.web','WEB294.web','WEB296.web','WEB296.web','WEB320.web','WEB320.web','WEB321.web','WEB321.web','WEB322.web','WEB322.web','WEB323.web','WEB323.web','WEB324.web','WEB324.web','WEB333.web','WEB333.web','WEB339.web','WEB339.web','WEB37.web','WEB38.web','WEB39.web','WEB39.web','WEB60.web','WEB60.web']
# # Taking too long to recalculate. Will be better to just
# # read in the datafile and trim these.

def removewebs(infile,outfile):
  goodlines=[]
  f=open(infile,'r')
  for line in f:
    if line.split()[0] not in dubious_webs:
      goodlines.append(line)
  f.close()

  g=open(outfile,'w')
  for line in goodlines:
    g.write(line)
  g.close()

def main():
  
  suffix = 'summary-properties_extended_connected.tsv'
  for indir in ['../mod_data/', '../non_TS/']:
    infile=indir+suffix
    outfile=indir+'summary-properties_trimmed.tsv'

    removewebs(infile,outfile)
    print infile, ' done'

if __name__ == '__main__':
  main()
