# Create a library called "libTShield"
enable_language (C CXX Fortran)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

set(INCLUDE_DIRECTORIES
 ${CMAKE_CURRENT_SOURCE_DIR}/inc
 ${TSHIELD_ROOT_PATH}/shield/inc
 ${TSHIELD_ROOT_PATH}/hadgen/inc
 ${TSHIELD_ROOT_PATH}/THadgen/inc
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

link_directories(
 ${ROOT_LIBRARY_DIR}
 ${FAIRROOT_LIBRARY_DIR}
)

configure_file(inc/TShield.h inc/TShield.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)

# List of source files
set(SRCS
    src/TShield.cxx
    src/TShieldCallbacks.cxx
    src/TShieldOpt.cxx
    src/TShieldGeometry.cxx
    src/TShieldGeometryBool.cxx
    src/TShieldGeometryConvert.cxx
    src/TShieldGeometryOperators.cxx
    src/TShieldGeometryPrint.cxx
)

set(HEADERS inc/TShield.h)

Set(LINKDEF inc/LinkDef.h)
Set(LIBRARY_NAME TShield)
Set(DEPENDENCIES SHIELD HADGEN Core Base)

GENERATE_LIBRARY()

