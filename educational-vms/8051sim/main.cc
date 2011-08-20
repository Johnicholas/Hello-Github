//-----------------------------------------------------------------------------

// Roman Lysecky, Tony Givargis, Greg Stitt, Ann Gordon-Ross, and 
// Kris Miller 
//
// Copyright (C) 2001, All Rights Reserved.

//-----------------------------------------------------------------------------

// 8051 Instruction Set Simulator 
// Version 1.4

//-----------------------------------------------------------------------------

#include <iostream>
#include <signal.h>
#include "i8051.h"

//-----------------------------------------------------------------------------

void SigHandler(int signal);

//-----------------------------------------------------------------------------

static I8051 i8051;

//-----------------------------------------------------------------------------

int 
main(int argc, char* argv[]) 
{     
    
    // check for too many paramters
    if( argc > 3 ) {
      std::cerr << "usage: 8051sim <HEX File> <Trace File>\n";
	exit(0);  
    }
    else {
	signal(SIGINT, SigHandler);

 	i8051.Simulate(argc>=2 ? argv[1] : "out.hex", 
		       argc==3 ? argv[2] : "output.txt");
    }
    
    return(0);
}

//-----------------------------------------------------------------------------

void SigHandler(int signal) 
{
    i8051.Stop();
}

//-----------------------------------------------------------------------------
