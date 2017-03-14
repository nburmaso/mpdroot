 ################################################################################
 #    Copyright (C) 2014 GSI Helmholtzzentrum fuer Schwerionenforschung GmbH    #
 #                                                                              #
 #              This software is distributed under the terms of the             # 
 #         GNU Lesser General Public Licence version 3 (LGPL) version 3,        #  
 #                  copied verbatim in the file "LICENSE"                       #
 ################################################################################
# Create a library called "libClustering" which includes the source files given in
# the array

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/clustering
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
) 

link_directories( ${LINK_DIRECTORIES})

set(MPDCLUSTER_SRCS
ClusterElement.cxx
Cluster.cxx
ClusterCollection.cxx
ClManager.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDCLUSTER_HEADERS "${MPDCLUSTER_SRCS}")

set(MPDCLUSTER_LINKDEF ClusterLinkDef.h)
set(MPDCLUSTER_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdClusterDict.cxx) 

ROOT_GENERATE_DICTIONARY("${MPDCLUSTER_HEADERS}" "${MPDCLUSTER_LINKDEF}" "${MPDCLUSTER_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

set(MPDCLUSTER_SRCS ${MPDCLUSTER_SRCS} ${MPDCLUSTER_DICTIONARY})

add_library(Cluster SHARED ${MPDCLUSTER_SRCS})
target_link_libraries(Cluster ${ROOT_LIBRARIES})
set_target_properties(Cluster PROPERTIES  ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Cluster DESTINATION ${CMAKE_BINARY_DIR}/lib)

