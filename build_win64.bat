if not exist ".\build\" mkdir .\build

cl -std:c17 /Fe:.\build\nibble.exe .\src\main.c 
