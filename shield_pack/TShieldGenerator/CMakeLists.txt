enable_language (CXX)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") #Used for initializer lists at TShieldGeometry
set(INCLUDE_DIRECTORIES ${ROOT_INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/inc ${TSHIELD_ROOT_PATH}/hadgen/inc ${TSHIELD_ROOT_PATH}/shield/inc ${TSHIELD_ROOT_PATH}/THadgen/inc ${TSHIELD_ROOT_PATH}/TShield/inc)
include_directories(${INCLUDE_DIRECTORIES})
link_directories(${ROOT_LIBRARY_DIR})

configure_file(inc/TShieldGenerator.h inc/TShieldGenerator.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)
set(G__ShieldGenerator_HEADERS inc/TShieldGenerator.h)
set(G__ShieldGenerator_LINKDEF inc/LinkDef.h)
set(G__ShieldGenerator_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/G__ShieldGenerator.cxx)
ROOT_GENERATE_DICTIONARY("${G__ShieldGenerator_HEADERS}" "${G__ShieldGenerator_LINKDEF}" "${G__ShieldGenerator_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

set(TShieldGenerator_SRCS
    src/TShieldGenerator.cxx
    ${G__ShieldGenerator_DICTIONARY}
)

add_library(TShieldGenerator SHARED ${TShieldGenerator_SRCS})
target_link_libraries(TShieldGenerator TShield THadgen ${ROOT_LIBRARIES})

