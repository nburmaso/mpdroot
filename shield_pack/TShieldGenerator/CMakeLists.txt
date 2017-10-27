# Create a library called "libTShieldGenerator"
enable_language (CXX)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") #Used for initializer lists at TShieldGeometry

set(INCLUDE_DIRECTORIES
 ${CMAKE_CURRENT_SOURCE_DIR}/inc
 ${TSHIELD_ROOT_PATH}/hadgen/inc
 ${TSHIELD_ROOT_PATH}/shield/inc
 ${TSHIELD_ROOT_PATH}/THadgen/inc
 ${TSHIELD_ROOT_PATH}/TShield/inc
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

link_directories(${ROOT_LIBRARY_DIR})

configure_file(inc/TShieldGenerator.h inc/TShieldGenerator.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)

# List of source files
set(SRCS
    src/TShieldGenerator.cxx
)

set(HEADERS inc/TShieldGenerator.h)

Set(LINKDEF inc/LinkDef.h)
Set(LIBRARY_NAME TShieldGenerator)
Set(DEPENDENCIES TShield THadgen Core Base)

GENERATE_LIBRARY()
