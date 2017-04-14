# Create a library called "libMpdBase" which includes the source files given in
# the array.
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR}
${CLHEP_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/mpdbase
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

# List of source files
set(MPDBASE_SRCS
MpdHelix.cxx
MpdCodeTimer.cxx
)

if(RULE_CHECKER_FOUND)
  CHECK_RULES("${MPDBASE_SRCS}" "${INCLUDE_DIRECTORIES}" MPDBASE_RULES)
endif(RULE_CHECKER_FOUND)

SET_SOURCE_FILES_PROPERTIES(${MPDBASE_SRCS} PROPERTIES COMPILE_FLAGS
  "-O3 ")


# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDBASE_HEADERS "${MPDBASE_SRCS}")

# set everything needed for the root dictonary and create the
# dictionary
set(MPDBASE_LINKDEF MpdBaseLinkDef.h)
set(MPDBASE_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdBaseDict.cxx) 
ROOT_GENERATE_DICTIONARY("${MPDBASE_HEADERS}" "${MPDBASE_LINKDEF}" "${MPDBASE_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

# add the dictionary to the list of source files
SET(MPDBASE_SRCS ${MPDBASE_SRCS} ${MPDBASE_DICTIONARY}) 


############### build the library #####################
add_library(MpdBase SHARED ${MPDBASE_SRCS})
target_link_libraries(MpdBase ${ROOT_LIBRARIES})
set_target_properties(MpdBase PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

############### install the library ###################
install(TARGETS MpdBase DESTINATION ${CMAKE_BINARY_DIR}/lib)
