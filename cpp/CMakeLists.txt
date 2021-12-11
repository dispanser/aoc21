cmake_minimum_required(VERSION 3.20) # this is required. use a recent one, I guess

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")

# https://olddiscourse.nixos.org/t/get-clangd-to-find-standard-headers-in-nix-shell/11268/10
if(CMAKE_EXPORT_COMPILE_COMMANDS)
  set(CMAKE_CXX_STANDARD_INCLUDE_DIRECTORIES 
      ${CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES})
endif()

project (aoc21
	VERSION 1.0
	DESCRIPTION "advent of code"
	LANGUAGES CXX)                      # name of project... 
add_executable(01 src/day01.cpp)
add_executable(02 src/day02.cpp)
