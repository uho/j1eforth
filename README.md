j1eforth
========

eForth model for the j1 CPU simulator

Compile simulator on windows with MinGW

gcc j1.c -o j1.exe

Compile eForth using an existing gforth installation or any ANS compatible FORTH to genearte the j1.bin

gforth j1.f

Run the simulator

j1.exe

or

j1.exe [argument] , where [argument] is any forth file to be copiled by the simulator into the dictionary.

enjoy ..

Wrds to test in the simulatorsee , ' , compile , [compile] , ?branch , branch , call, .. and many more ]

Have fun , modify and pass on , ( I would like to see it running on the actual FPGA of J1 if possible



