# This is the CMake build file.
#
# George Gayno

set(fortran_src
    fix_coasts.f90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -check bounds")
endif()

set(exe_name fix_coasts.exe)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  g2::g2_d
  bacio::bacio_4
  w3nco::w3nco_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${exec_dir})
