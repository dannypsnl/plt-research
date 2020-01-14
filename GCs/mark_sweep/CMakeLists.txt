cmake_minimum_required(VERSION 3.10)

project(mark_sweep)

add_compile_options(-std=c++14)

add_library(mark_sweep_lib src/msobject.cc)

add_executable(mark_sweep src/main.cc)
target_link_libraries(mark_sweep
        mark_sweep_lib)

file(GLOB_RECURSE TEST_CODES src/*_test.cc)
enable_testing()
find_package(GTest REQUIRED)
add_executable(tests ${TEST_CODES})
target_link_libraries(tests
        GTest::GTest GTest::Main
        mark_sweep_lib # mark sweep GC library
        )
add_test(
        NAME test_mark_sweep
        COMMAND tests
)
