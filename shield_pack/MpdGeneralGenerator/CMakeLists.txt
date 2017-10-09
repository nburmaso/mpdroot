enable_language (CXX)

set(INCLUDE_DIRECTORIES
    ${ROOT_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/inc
    ${CMAKE_SOURCE_DIR}/fairtools
    ${CMAKE_SOURCE_DIR}/geobase
    ${CMAKE_SOURCE_DIR}/base/sim
    ${CMAKE_SOURCE_DIR}/generators
    ${CMAKE_SOURCE_DIR}/gconfig
)
if(PLUTO_FOUND)
set(INCLUDE_DIRECTORIES ${INCLUDE_DIRECTORIES} ${PLUTO_INCLUDE_DIR})
EndIf(PLUTO_FOUND)

include_directories(${INCLUDE_DIRECTORIES})

if(PLUTO_FOUND)
link_directories(${ROOT_LIBRARY_DIR} ${PLUTO_LIBRARY_DIR})
elseif(PLUTO_FOUND)
link_directories(${ROOT_LIBRARY_DIR})
EndIf(PLUTO_FOUND)

configure_file(inc/MpdGeneralGenerator.h inc/MpdGeneralGenerator.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)
set(G__MpdGeneralGenerator_HEADERS inc/MpdGeneralGenerator.h)
set(G__MpdGeneralGenerator_LINKDEF inc/LinkDef.h)
set(G__MpdGeneralGenerator_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/G__MpdGeneralGenerator.cxx)
ROOT_GENERATE_DICTIONARY("${G__MpdGeneralGenerator_HEADERS}" "${G__MpdGeneralGenerator_LINKDEF}" "${G__MpdGeneralGenerator_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

set(MpdGeneralGenerator_SRCS
    src/MpdGeneralGenerator.cxx
    ${G__MpdGeneralGenerator_DICTIONARY}
)

add_library(MpdGeneralGenerator SHARED ${MpdGeneralGenerator_SRCS})
target_link_libraries(MpdGeneralGenerator ${ROOT_LIBRARIES})

