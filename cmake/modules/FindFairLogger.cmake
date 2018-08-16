# Find FairLogger installation
# Once done this will define
#  FAIRLOGGER_FOUND - system has found FairLogger installation
#  FAIRLOGGER_INCLUDE_DIR - FairLogger include directory
#  FAIRLOGGER_LIBRARY_DIR - FairLogger library directory

find_path(FAIRLOGGER_INCLUDE_DIR NAMES fairlogger/Logger.h PATHS
  ${FAIRLOGGER_ROOT}/include  
  ${FAIRROOTPATH}/include
  NO_DEFAULT_PATH
)

find_path(FAIRLOGGER_LIBRARY_DIR NAMES libFairLogger.so PATHS
  ${FAIRLOGGER_ROOT}/lib
  ${FAIRROOTPATH}/lib
  NO_DEFAULT_PATH
)

if (FAIRLOGGER_INCLUDE_DIR AND FAIRLOGGER_LIBRARY_DIR)
  set(FAIRLOGGER_FOUND TRUE)
  SET(LD_LIBRARY_PATH ${LD_LIBRARY_PATH} ${FAIRLOGGER_LIBRARY_DIR})
  if (NOT FAIRLOGGER_FIND_QUIETLY)
    message(STATUS "Looking for FairLogger... found at ${FAIRLOGGER_LIBRARY_DIR}")
  endif (NOT FAIRLOGGER_FIND_QUIETLY)
else (FAIRLOGGER_INCLUDE_DIR AND FAIRLOGGER_LIBRARY_DIR)
  set(FAIRLOGGER_FOUND FALSE)
  if (FAIRLOGGER_FIND_REQUIRED)
    message(FATAL_ERROR "Looking for FairLogger... not found")
  else (FAIRLOGGER_FIND_REQUIRED)
    message(STATUS "Looking for FairLogger... not found")
  endif (FAIRLOGGER_FIND_REQUIRED)
endif (FAIRLOGGER_INCLUDE_DIR AND FAIRLOGGER_LIBRARY_DIR)
