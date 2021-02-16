# CCTV

Type function
Title CCTV
Version 1
Date 2020-05-1
Author Marianna Milano,
Maintainer Marianna Milano <m.milano@unicz.it>, 
Description An R-Package R function for graph-based analysis and visualization of Italian COVID-19 data.
License GPL (>=2)
Depends R (>= 3.1.0),igraph package.

 CCTV is a R function for graph-based analysis and visualization of Italian COVID-19 data.
The dataset, that is provided for each Italian region, consist of:
Hospitalised with Symptoms (hs)
Intensive Care (ic)
Total Hospitalised (th)
Home Isolation (hi)
Total Currently Positive (tcp)
New Currently Positive (ncp)
Discharged/ Healed (dh)
Deceased (d)
Total Cases (tc)
 Swabs (sw)

An example of dataset is reported in dataset.txt file.


CCTV function takes as input a list of Italin Region associated with  Hospitalised with Symptoms (recall with id:hs)
Intensive Care (recall with id:ic), Total Hospitalised (recall with id:th), Home Isolation (recall with id:hi), Total Currently Positive (recall with id:tcp), New Currently Positive (recall with id:ncp), Discharged/ Healed (recall with id:dh), Deceased (recall with id:d),Total Cases (recall with id:tc),Swabs (recall with id:sw).
The user selects the id according to the type of data that he/she want analyze.
AnalyzeC19D function  builts a similarity matrix M(i,j) where the (i,j) value of the matrix for data
204 k (e.g. swab data) represents the p-value of the Wilcoxon statistical test obtained by performing the
205 test on the swab measures of region i with respect to region j. Lower p-value means that regions are
206 more dissimilar with respect to that measure. Higher p-value means that regions are more similar with
207 respect to that measure. We used the usual significance threshold of 0.05, thus matrices report only
208 p   vales >= 0.05, while p   values < 0.05 are mapped to zero. In such a way the p-value is used as a
209 similarity measure.
CCTV function write the similarity matrix in  simR.txt.
After that, CCTV function  maps each matrix M(i,j) to a network N starting from the result of Wilcoxon test. The nodes of the networks are the Italian regions and the edges connects two regions (i,j) if the p-value in
 the similarity matrix is greater than the threshold, otherwise (p-value < 0.05) there is not connection
 among nodes.
 CCTV enables the visualization of the network.
 Finally, CCTV  detects the communities on the network by applying an Walktrap community detection algorithm.  CCTV enables the visualization of the communities.
