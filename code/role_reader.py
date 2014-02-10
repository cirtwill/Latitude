#!/usr/bin/python


# system libraries
import sys
import pymfinder
import os

#pymfinder.link_roles(sys.argv[1])

def printroles(infile,stoufferIDs=True):
  roles=pymfinder.motif_roles(infile) #Motif roles for each species

  name=infile.split('/')[-1]
  pymfinder.print_roles(roles,3,outFile='Food-web-database/Roles/'+name+'.roles')

def printmotifsummary(infile,stoufferIDs=True):
  motifs=pymfinder.motif_structure(infile)

  name=infile.split('/')[-1]
  print name
  outy='Food-web-database/Roles/'+name+'.motifs'
  pymfinder.print_motif_structure(motifs,outFile=outy)



def printlinks(infile,stoufferIDs=True):
  linkroles=pymfinder.link_roles(infile) #Motif roles for each link.
  name=infile.split('/')[-1]
  pymfinder.print_link_roles(linkroles,3,outfile='Food-web-database/Roles/'+name+'.link_roles')

  #Achieve output by running python test.py >../Data/allroles.dat

def wrapper(directory):
  files=os.listdir(directory)
  for fil in files:
    printroles(directory+'/'+fil)
    printmotifsummary(directory+'/'+fil)


def main():
  directory='Food-web-database/pred-prey-lists-to-use'

  wrapper(directory)


if __name__ == '__main__':
  main()
