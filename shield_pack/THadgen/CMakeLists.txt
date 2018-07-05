# Create a library called "libTHadgen"
enable_language (C CXX Fortran)

set(INCLUDE_DIRECTORIES
 ${CMAKE_CURRENT_SOURCE_DIR}/inc
 ${TSHIELD_ROOT_PATH}/hadgen/inc
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

configure_file(inc/THadgen.h inc/THadgen.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)

# List of source files
set(SRCS
    src/THadgen.cxx
    src/THadgenRoutines.cxx
)

set(HEADERS inc/THadgen.h)

Set(LINKDEF inc/LinkDef.h)
Set(LIBRARY_NAME THadgen)
Set(DEPENDENCIES HADGEN Core Base)

GENERATE_LIBRARY()
