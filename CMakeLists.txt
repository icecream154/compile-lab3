cmake_minimum_required(VERSION 3.20)
project(lab3)

set(CMAKE_CXX_STANDARD 14)
include_directories(/opt/homebrew/opt/llvm/include)
link_directories(/opt/homebrew/opt/llvm/lib)

add_executable(lab3
        lab3.cpp
        main.cpp)
