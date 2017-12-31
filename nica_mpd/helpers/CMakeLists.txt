# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
Set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/nica_mpd/helpers
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR} 
)

Set(LINK_DIRECTORIES
  ${NICA_LINK_DIRECTORIES}
)

Include_Directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})
 
Link_Directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
MpdNicaHelix.cxx
)

Set(HEADERS )
Set(LINKDEF NicaMpdHelpersLinkDef.h)
Set(LIBRARY_NAME NicaMpdHelpers)
Set(DEPENDENCIES NicaFeatures NicaDataFormat)

GENERATE_LIBRARY()


