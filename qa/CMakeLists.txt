
# Create a library called "MpdPhysics" which includes the source files given in the array.
# The extension is already found. Any number of sources could be listed here.
set(INCLUDE_DIRECTORIES
 ${BASE_INCLUDE_DIRECTORIES}
 ${CMAKE_SOURCE_DIR}/mpddst
 ${CMAKE_SOURCE_DIR}/mpddst/MpdMiniEvent
 ${CMAKE_SOURCE_DIR}/mpdst/mcDst
 ${CMAKE_SOURCE_DIR}/nicafemto/features
 ${CMAKE_SOURCE_DIR}/nicafemto/features/hist
 ${CMAKE_SOURCE_DIR}/nicafemto/features/io
 ${CMAKE_SOURCE_DIR}/nicafemto/features/io/html
 ${CMAKE_SOURCE_DIR}/nicafemto/features/std
 ${CMAKE_SOURCE_DIR}/nicafemto/dataformat
 ${CMAKE_SOURCE_DIR}/nicafemto/dataformat/detector
 ${CMAKE_SOURCE_DIR}/nicafemto/dataformat/compound
 ${CMAKE_SOURCE_DIR}/nicafemto/analysis/qa
 ${CMAKE_SOURCE_DIR}/qa
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
 ${GSL_INCLUDE_DIRS}
 ${CLHEP_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
 ${FAIRROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
 MpdMiniDstEventQA.cxx
 MpdMiniDstQA.cxx
 MpdMiniDstQATask.cxx
 MpdMiniDstTrackQA.cxx
)

Set(HEADERS)
Set(LINKDEF MpdQALinkDef.h)
Set(LIBRARY_NAME MpdQA)
Set(DEPENDENCIES Core Base NicaFeatures NicaQA)

GENERATE_LIBRARY()
