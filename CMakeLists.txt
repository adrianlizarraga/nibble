cmake_minimum_required(VERSION 3.20)
project(nibble)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-Wall -Wextra -pedantic)

add_executable(nibble src/main.c)
target_include_directories(nibble PUBLIC "${PROJECT_SOURCE_DIR}/src")
