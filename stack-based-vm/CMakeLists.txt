cmake_minimum_required(VERSION 3.15)
project(stack_based_vm)

set(CMAKE_CXX_STANDARD 17)

add_executable(stack_based_vm main.cpp Value.h Value.cpp)