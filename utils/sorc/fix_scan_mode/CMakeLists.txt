# This is the CMake build file.
#
# George Gayno

set(fortran_src
    fix_scan_mode.f90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -check bounds")
endif()

set(exe_name fix_scan_mode.exe)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  g2::g2_d
  bacio::bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${exec_dir})
