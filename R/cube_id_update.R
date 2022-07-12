#' This script is used to link the most recent version of the v832 TDM node ids with the new v9 TDM node ids.
#' Linking the old node ids with the new ones is needed in order to connect the two networks. This connection is
#' helpful now while transitioning to the new network and may be helpful in the future if we need to get data from
#' the old network.
#'
#'@param v9_nodes An exported dbf file of the v9 network nodes.
#'@param v832_nodes An exported dbf file of the most recent v832 network nodes.
#'
#'@return A csv file containing the node id of the v832 network linked with the v9 network node ids.
#'
#'@details This script is useful for this particular task, but probably isn't a sustainable methodology. In the future,
#'a new script will probably have to be constructed again. Many parts of this script are exclusive to the specific node
#'ids that currently exist.
#'
#'@author Chris Day
#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

#' needed libraries
library(tidyverse)
library(readr)
library(foreign)

# read in both node tables from both networks
v9_nodes <- read.dbf("data/MasterNet - 2022-05-25.dbf")
v832_nodes <- read.dbf("data/Master_2022-03-22_new.dbf") %>%
  select(N,X,NEWX,Y,NEWY,GEOGKEY) %>% rename("N_v832_correct" = N)

# join by GEOGKEY (Xcoord + "_" + Ycoord)
# since some nodes were moved between networks, GEOGKEY doesn't match all of the node ids together
v9_v832_nodes <- left_join(v9_nodes,v832_nodes,by = c("GEOGKEY")) %>%
  select(N,N_V832,N_V9,N_v832_correct) %>%
  # develop a guess for node connection by simply subtracting by 10000 or using the same node id
  mutate(N_v832_guess = ifelse(N < 2890, N, ifelse(N < 100000, N - 10000, N)))


# determine nodes that exist in v832, but don't match up by GEOGKEY or don't exist in v9
moved_nodes <- left_join(v832_nodes,v9_nodes, by = c("GEOGKEY")) %>%
  filter(is.na(N)) %>%
  select(N_v832_correct) %>%
  mutate(moved_node = N_v832_correct)
# nodes that did not line up with v9 nodes, but their "guess" did exist
moved_nodes_good_guess <- moved_nodes %>%
  filter(moved_node %in% v9_v832_nodes$N_v832_guess)
# nodes that did not line up with v9 nodes, and their "guess" did not exist either
moved_nodes_bad_guess <- moved_nodes %>%
  filter(!(moved_node %in% v9_v832_nodes$N_v832_guess))

# there are 167 cases where a node exists in v832 but needs manual inspection to determine v9 node
# use 0 to denote those nodes that exist in v832 but not in v9 (or are too confusing)
odd_cases <- data.frame(
  N_v832_manual = c(3679, 3687,  3798,  3933,  4270, 4276, 4299,  4452, 4479,  4567, 4720,  4730,  4775,  4814,  5081,  5203, 5231, 5463, 5794,
                     5799, 5895,  5955, 5963, 5992,  6000,   6073, 6074, 6079,  6130, 6139,  6201, 6324,  6366,  6414,  6419,  6461,  6468, 
                     6551,  6554,  6567,  6586,  6588, 6590,  6609,  6611, 6694,  6698,  6723,  6727,  6729,  6751,  6761,  6762,  6763, 6764,
                     6770,  6772,  6816,  6866, 6894, 7010, 7031, 7090,  7095,  7111, 7135, 7137, 7179,  7197,  7354, 7458, 7459, 7466, 7600,
                     7608, 7777,  7891, 7904, 8011, 8088, 8203, 8224, 8260,  8286, 8311, 8357,  8394,  8429,  8550,  8551,  8586,  8606,  8607,
                     8632, 8656,  8658,  8727,  8748,  8840,  8843, 8870,  8874,  8876,  9010, 9279, 9523, 9524, 9525, 9526, 9527, 9535, 9547,
                     9549,  9552, 9581,  9589,  9608, 9628, 9656,  9670, 9949,  10778, 10896, 10992, 11057, 11289, 11727, 12033, 12156, 12225,
                     12245, 12257, 13114, 13157, 20148, 20465, 20843, 20941, 21856, 22013, 22015, 22322, 22442, 22571, 22665, 22666, 23234, 
                     23351, 23544, 23736, 23745, 24008, 24276, 25158, 25159,  25160,  26037, 26711, 26774, 26775, 30308, 30834, 30847, 30874,
                     30975, 31666, 31670),
  N =             c(1046, 36094, 35970, 34967, 0,    0,    36358, 0   , 34754, 0,    34479, 34494, 32153, 33489, 33734, 102116,    0,    0,    0,
                     0,    31618, 0,    0,    38391,  38374, 0,    0,    38548, 0,    38413, 0,    39383, 37823, 36978, 37260, 37939, 36783, 
                     37595, 38439, 39145, 39245, 0,    38019, 38090, 0,    38628, 38299, 38242, 38695, 38294, 38698, 38338, 38373, 0,    38687,
                     38780, 38509, 38807, 0,    0,    0,    0,    39155, 39190, 0,    0,    0,    38253, 39196, 0,    0,    0,    0,    0,
                     0,    38334, 0,    0,    0,    0,    0,    0,    39053, 0,    0,    38481, 39106, 37748, 38249, 38114, 37872, 38405, 37993,
                     0,    38007, 37915, 37934, 39207, 38778, 0,    39184, 39165, 0,    0,    0,    0,    0,    0,     0,    0,    38853, 38891,
                     38859, 38836, 38601, 38645, 0,    37550,    37330, 0,    33761, 38895, 38919, 0,     0,     0,     0,     0,     0,     36784,
                     34816, 0,     0,     0,     0,     0,     75029, 0,     0,     79654, 81596, 0,     0,     77923, 77945, 81593, 81522, 
                     81000, 75028, 75776, 0,     79652, 81001, 0,     165005, 103616, 0,     0,     0,    0,     0,     0,     0,     0,
                     0, 25030, 0)
) %>%
  mutate(description = ifelse(N == 0, "Node Not in v9",NA))

# write csv of nodes that aren't showing up in v9 network
Forgotten_Nodes <- odd_cases %>% filter(N ==0)
write_csv(Forgotten_Nodes,"forgotten_nodes.csv")

# determine the v832 node that best fits the v9 node and write a description for how that id was determined
descrip <- v9_v832_nodes %>%
  left_join(moved_nodes_good_guess, by = c("N_v832_guess" = "N_v832_correct")) %>%
  left_join(odd_cases, by = c("N")) %>%
  mutate(description = case_when(
    !is.na(N_v832_correct) ~ "GEOGKEY matches",
    N < 2890 ~ "Moved Centroid Node",
    !is.na(N_v832_manual) ~ "Moved Node, Bad Guess",
    !is.na(moved_node) ~ "Moved Node, Good Guess"
  )) %>%
  mutate(N_v832_correct = ifelse(description == "GEOGKEY matches", N_v832_correct, 
                          ifelse(description == "Moved Centroid Node", N_v832_guess,
                          ifelse(description == "Moved Node, Bad Guess", N_v832_manual,
                          ifelse(description == "Moved Node, Good Guess", moved_node, N_v832_correct))))
  ) %>%
  select(-N_v832_guess,-moved_node,-N_v832_manual)

# write csv of node key
write_csv(descrip, "node_key_v832_to_v9.csv")

sum(is.na(descrip$N_v832_correct))

# v9_nodes has 1722 more nodes than v832_nodes
# there are now 1800 nodes that are not specified
# BUT there are 77 nodes in the v832 network that don't exist in the v9 network

# There is one NA value that shouldn't be NA I think. But I can't find it right now.