cmake_minimum_required(VERSION 3.6)
project(SignalProvider VERSION 2.0.1 LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/ftnmods")

# Static MSVC runtime (/MT in Release, /MTd in Debug) - relevant when MSVC links
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

message(STATUS "CMAKE_Fortran_COMPILER_ID = ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

# NOTE:
# - With Visual Studio + Ninja (single-config), VS passes -DCMAKE_BUILD_TYPE=<...>
# - With Visual Studio generator (multi-config), CMAKE_BUILD_TYPE is ignored.
# So: don't FORCE it. Provide a default only for single-config command-line use.
if (NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING
      "Choose the build type: Debug RelWithDebInfo Release" )
endif()

set(SOURCES
    src/SignalProvider_Types.f90
    src/SignalProvider_Helpers.f90
    src/SignalProvider_Subs.f90
    src/SignalProvider.f90
)

add_library(SignalProvider SHARED ${SOURCES})

##### Platform/compiler specific flags ########################################

if(APPLE OR UNIX)
  # Enable .dll export / build options
  if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    target_compile_options(SignalProvider PRIVATE
      -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -cpp -no-wrap-margin
    )
  else()
    target_compile_options(SignalProvider PRIVATE
      -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8 -cpp
    )
  endif()

elseif (WIN32)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    target_compile_options(SignalProvider PRIVATE
      -ffree-line-length-0 -static-libgcc -static-libgfortran -static
      -fdefault-real-8 -fdefault-double-8 -cpp
    )

  elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # ifx/ifort on Windows uses slash options, not dash options
    target_compile_options(SignalProvider PRIVATE
      /libs:static
      /free
      /fpp
      /real-size:64
      /double-size:64
    )

    # Optional: set optimization per config explicitly (CMake usually does this,
    # but adding it makes intent clear and avoids surprises)
    target_compile_options(SignalProvider PRIVATE
      $<$<CONFIG:Debug>:/Od>
      $<$<CONFIG:Release>:/O2>
      $<$<CONFIG:RelWithDebInfo>:/O2>
    )
  endif()
endif()
###############################################################################
