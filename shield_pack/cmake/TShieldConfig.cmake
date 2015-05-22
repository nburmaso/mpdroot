set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
if(NOT DEFINED SIMPATH)
    set(SIMPATH $ENV{SIMPATH})
    set(ROOTSYS ${SIMPATH})
    set(ROOT_LIBRARY_DIR "${ROOTSYS}/lib/root")
    set(ROOT_INCLUDE_DIR "${ROOTSYS}/include/root")
    set(ROOT_CINT_EXECUTABLE "${SIMPATH}/bin/rootcint")
    set(LIBRARY_OUTPUT_PATH "${CMAKE_BINARY_DIR}/lib")
    set(configfile "config.sh")
    configure_file(cmake/config.sh.in ${configfile})

    # Debug flags
    set(CMAKE_C_FLAGS_DEBUG "-g -O0")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
    set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
    # Release flags
    set(CMAKE_C_FLAGS_RELEASE "-O2")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set(CMAKE_CXX_FLAGS_RELEASE "-O2")
    # RelWithDebInfo flags
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "-02 -g")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-02 -g")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-02 -g")
    #MinSizeRel flags
    set(CMAKE_C_FLAGS_MINSIZEREL "-Os")
    set(CMAKE_Fortran_FLAGS_MINSIZEREL "-Os")
    set(CMAKE_CXX_FLAGS_MINSIZEREL "-Os")

    if(NOT CMAKE_BUILD_TYPE)
        message(STATUS "CMAKE_BUILD_TYPE not set, default value is Release.")
        message(STATUS "To change CMAKE_BUILD_TYPE set it with -DCMAKE_BUILD_TYPE=\"version\".")
        message(STATUS "Avaible \"version\"s is \"None\", \"Debug\", \"Release\", \"RelWithDebInfo\", \"MinSizeRel\".")
        SET(CMAKE_BUILD_TYPE Release CACHE STRING
          "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
          FORCE)
    else()
        message(STATUS "Current CMAKE_BUILD_TYPE is \"${CMAKE_BUILD_TYPE}\".")
    endif()

    include(IndependentMacro)
endif()

if(NOT SIMPATH)
    message(FATAL_ERROR "Fairsoft installation not found, please provide SIMPATH first")
endif()
