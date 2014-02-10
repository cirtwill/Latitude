import sys
import re
import os
from math import *
from food_web_properties import *

def roleinfo(prefile_r,postfile_r):
  roledict={'pre':{},'post':{}}
  for fil in [prefile_r,postfile_r]:
    f=open(fil)
    for line in f:
      species=int(line.split()[0])
      role = line.split()[1:]
      if fil==prefile_r:
        roledict['pre'][species]=role
      elif fil==postfile_r:
        roledict['post'][species]=role        
    f.close()
  return roledict

def species_decoder(prefile_r,postfile_r):

  pre_spfile='Food-web-database/species-keys/all-webs/WEB278'
  post_spfile='Food-web-database/species-keys/all-webs/WEB279'
  species_namedict={'pre':{},'post':{}}
  f=open(pre_spfile)
  for line in f:
    num=int(line.split('\t')[1][:-1])
    name=line.split('\t')[0]
    species_namedict['pre'][num]=name
  f.close()

  g=open(post_spfile)
  for line in g:
    num=int(line.split('\t')[1][:-1])
    name=line.split('\t')[0]
    species_namedict['post'][num]=name
  g.close()

  return species_namedict


def distancer(prefile_r,postfile_r):
  species_namedict=species_decoder(prefile_r,postfile_r)

  roles_byname={'pre':{},'post':{}}

  roledict=roleinfo(prefile_r,postfile_r)
  for key in roledict:
    for species in roledict[key]:
      roles_byname[key][species_namedict[key][species]]=roledict[key][species]

  specieslist=[]
  for species in roles_byname['pre']:
    if species in roles_byname['post']:
      specieslist.append(species)


  distdict={}
  for species in specieslist:
    prerole=roles_byname['pre'][species]
    postrole=roles_byname['post'][species]
    delta=[]
    nonzero=[]
    for i in range(0,len(prerole)):
      delta.append((int(prerole[i])-int(postrole[i]))**2)
      if prerole[i]!='0':
        nonzero.append(prerole[i])
    dist=sqrt(sum(delta))
    distdict[species]=((dist,len(nonzero)))

  outfile=open('distances_after_invasion.tsv','w')
  for species in distdict:
    outfile.write(species+'\t'+str(distdict[species][0])+'\t'+str(distdict[species][1])+'\n')
  outfile.close()


  return distdict

def traitextractor(prefile_r,postfile_r):
  preweb=create_web('Food-web-database/pred-prey-lists/','WEB278')


  print preweb.nodes()


def motifinfo(prefile,postfile):

  motifdict={'pre':{},'post':{}}

  for fil in [prefile,postfile]:
    f=open(fil)
    for line in f:
      motif=line.split()[0]
      count=line.split()[1]
      if fil==prefile:
        motifdict['pre'][motif]=count
      else:
        motifdict['post'][motif]=count
    f.close()

  Rfile=open('Invasion_motif_structure.tsv','w')
  Rfile.write('webtype')
  for motif in sorted(motifdict['pre'].keys()):
    Rfile.write('\t'+motif)
  Rfile.write('\n')
  Rfile.write('pre')
  for motif in sorted(motifdict['pre'].keys()):
    Rfile.write('\t'+motifdict['pre'][motif])
  Rfile.write('\n')
  Rfile.write('post')
  for motif in sorted(motifdict['post'].keys()):
    Rfile.write('\t'+motifdict['post'][motif])
  Rfile.write('\n')
  Rfile.close()
  print 'done'

def main():
  prefile='Food-web-database/Roles/WEB278.motifs'
  postfile='Food-web-database/Roles/WEB279.motifs'

  prefile_r='Food-web-database/Roles/WEB278.roles'
  postfile_r='Food-web-database/Roles/WEB279.roles'

  distancer(prefile_r,postfile_r)
  #species_decoder(prefile_r,postfile_r)
  #roleinfo(prefile_r,postfile_r)

if __name__ == '__main__':
  main()

# #For whole webs. Probably not useable - sample size etc.
# library(vegan)
# counts=as.data.frame(with(data,cbind(X102,X108,X110,X12,X14,X238,X36,X38,X46,X6,X74,X78,X98)))
# row.names(counts)=row.names(data)
# data$total=rowSums(counts)
# D=as.dist(1-cor(t(counts)))  #distance 

# #Testing for differences in mean role vectors (confounded with dispersal)
# prelim=with(data,adonis(D~webtype,permutations=999))  #Combining species type and web type.
# write.table(prelim$aov.tab,file="../Webs/R_Output/Motif_Outputs/Allsites/type_mean_anova.tsv",sep='\t')

