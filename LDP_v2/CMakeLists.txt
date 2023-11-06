cmake_minimum_required(VERSION 3.6)
project(LDP_v2 VERSION 2.0.1 LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/ftnmods")

##### from ROSCO ################################################
if (NOT CMAKE_BUILD_TYPE)
  # Sets the optimization level to -O2 and includes -g for debugging
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the build type: Debug RelWithDebInfo Release" FORCE)
endif()

message(STATUS "CMAKE_Fortran_COMPILER_ID = ${CMAKE_Fortran_COMPILER_ID}")
if(APPLE OR UNIX)
# Enable .dll export
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -cpp -no-wrap-margin")
else()
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8 -cpp")  
endif()
elseif (WIN32)
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
# Ensure static linking to avoid requiring Fortran runtime dependencies
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8 -cpp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -libs:static -free -static -fpp -real-size:64 -double-size:64")
#     set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /ffree-line-length-0 /static-libgcc /static-libgfortran /static /fdefault-real-8 /fdefault-double-8 /cpp")
endif()
endif()
#################################################################

set(SOURCES
    src/LDP_v2.f90
    src/LDP_v2_Subs.f90
    src/LDP_v2_Types.f90
    src/LDP_Helpers.f90
)

add_library(LDP_v2 SHARED ${SOURCES})
