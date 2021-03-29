# This is the main CMake file.
#
# George Gayno
cmake_minimum_required(VERSION 3.15)

project(
  ufs_util
  LANGUAGES Fortran)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# User options.
option(OPENMP "use OpenMP threading" ON)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O0")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -r8 -check -check noarg_temp_created -check nopointer -fp-stack-check -fstack-protector-all -fpe0 -debug -ftrapuv")
endif()

find_package(w3nco REQUIRED)
find_package(bacio REQUIRED)
find_package(g2 REQUIRED)

set(exec_dir exec)

add_subdirectory(sorc)
