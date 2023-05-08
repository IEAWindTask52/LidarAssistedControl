cmake_minimum_required(VERSION 3.6)
project(LDP_v2 VERSION 2.0.1 LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/ftnmods")

# Sets the optimization level to -O2 and includes -g 
set(CMAKE_BUILD_TYPE "RelWithDebInfo")

# Enable .dll export
if(APPLE OR UNIX)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT ")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0")
  endif()
endif()

set(SOURCES
    src/LDP_v2.f90
    src/LDP_v2_Subs.f90
    src/LDP_v2_Types.f90
    src/LDP_Helpers.f90
)

add_library(LDP_v2 SHARED ${SOURCES})
