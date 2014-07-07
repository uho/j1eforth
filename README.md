eForth for the j1 simulator
========


Compile simulator on windows with MinGW

gcc j1.c -o j1.exe

Compile j1eForth using an existing gforth installation or any ANS compatible FORTH to genearte the j1.bin

gforth j1.4th

Run the simulator

j1.exe

or

j1.exe [argument] , where [argument] is any forth file to be compiled by the simulator into the dictionary.

enjoy ..

Words to test in the simulator [ see , ' , compile , [compile] , ?branch , branch , call, .. and many more ]

Have fun , modify and pass on , ( I would like to see it running on the actual j1 FPGA if possible )



