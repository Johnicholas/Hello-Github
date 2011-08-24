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
#include <iomanip>
#include <fstream>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "i8051.h"

using namespace std;

//-----------------------------------------------------------------------------

I8051::I8051() : InvalidData(11),
                 LineLength(80),
                 RecordTypeLength(3),
                 ACC(0x0160),
                 PSW(0x0150),
                 B(0x0170),
                 SP(0x0101),
                 P0(0x0100),
                 P1(0x0110),
                 P2(0x0120),
                 P3(0x0130),
                 DPL(0x0102),
                 DPH(0x0103),
                 PC(0),
                 instrCount(0),
                 cycleCount(0),
                 progEnd(false)
{
    // no code
}

//-----------------------------------------------------------------------------

I8051::~I8051() 
{
    // no code
}

//-----------------------------------------------------------------------------

bool 
I8051::Simulate(const char* inFile, const char* outFile) 
{
    ofstream os;

    clock_t begin, end;

    unsigned short tempProduct = 0;
    unsigned short jumpAddr;
    short tempAdd;
    unsigned char directAddr = 0;
    unsigned char regNum;
    unsigned char rotateBit;
    unsigned char lowerNibble;
    unsigned char tempACC;
    char temp;
    char popData;
    
    bool carry3;
    bool carry6;
    bool carry7;
    bool borrow3;
    bool borrow6;
    bool borrow7;

#ifdef PORTS
    char lastP0 = 0xFF;
    char lastP1 = 0xFF;
    char lastP2 = 0xFF;
    char lastP3 = 0xFF;
#endif
    
    if( !LoadHex(inFile) ) {
        return false;
    }
    else {
        #ifdef DEBUG
	    os.open(outFile);
	    if( os.bad() ) {
		cerr << "Error: bad output file." << endl;
		return false;
	    }
	#endif

        // initialize 8051 internal RAM
	Init8051();
	
	begin = clock();
	
	// program Loaded emulate program
	while(!progEnd) {
	    
	    if( PC >= RomSize ) PC = 0;
	    
	    #ifdef DEBUG
	    #ifdef DEBUG_PC
	        os << setw(5) << setfill('0') << PC << " - ";	       
            #endif
            #endif

	    // get instruction
	    IR = ROM[PC++];

	    // increment number of instructions executed
	    instrCount++;
	    
	    switch(Decode(IR)) {
		
		//(A) <- (A) & (Rn)
		case ANL1:
		    
		    #ifdef DEBUG
			os << "ANL 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A & R" << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[ACC] &= RAM[GetRegisterBank()+regNum];
		    cycleCount += 12;
		    break;
		    
		    //(A) <- (A) & (direct)		  
		case ANL2:
		    
		    #ifdef DEBUG
			os << "ANL 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A & RAM(" << (unsigned int)(unsigned char)ROM[PC] << ")" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] &= RAM[IR < 128 ? IR : (IR+128)];
		    cycleCount += 12;
		    break;
		    
		    //(A) <- (A) & ((Ri))
		case ANL3:
		    
 		    #ifdef DEBUG
			os << "ANL 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A & RAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[ACC] &= RAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 12;
		    break;
		    
		    //(A) <- (A) & (#data)
		case ANL4:
		
		    #ifdef DEBUG
			os << "ANL 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A & "; PrintHex(ROM[PC], &os); os << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] &= (char)IR;
		    cycleCount += 12;
		    break;
		
		    // (direct) <- (direct) & (A)
		case ANL5:
		
		    #ifdef DEBUG
			os << "ANL 5" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " & A" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)] &= RAM[ACC];
		    cycleCount += 12;
		    break;
		
		    //(direct) <- (direct) & (#data)
		case ANL6:
		
		    #ifdef DEBUG
			os << "ANL 6" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " & "; 
			PrintHex(ROM[PC+1], &os); os << endl;
		    #endif
		    #endif
		    directAddr = ROM[PC++];
		    IR = ROM[PC++];
		    RAM[directAddr < 128 ? directAddr : (directAddr+128)] &= 
			(char)IR;
		    cycleCount += 24;
		    break;

		    // (C) <- (C) & (bit)
		case ANL7:
		
		    #ifdef DEBUG
			os << "ANL 7" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- C & RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( !GetBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07)) ) {
			ClearBit(RAM[PSW], CY);
		    }
		    
		    /* if( (GetBit(RAM[PSW], CY) & 
		       GetBit(RAM[((IR & 0xF8) < 128) ? 
		       (((IR & 0xF8)>>3)+32) : 
		       (128 + (IR & 0xF8))], (IR & 0x07))) != 0x00 ) {
		       SetBit(RAM[PSW], CY);
		       }
		       else {
		       ClearBit(RAM[PSW], CY);
		       }*/
		    cycleCount += 24;
		    break;
		
		    // (C) <- (C) & /(bit)
		case ANL8:
		
		    #ifdef DEBUG
			os << "ANL 8" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- C & !RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07)) ) {
			ClearBit(RAM[PSW], CY);
		    }
		    /*if( (GetBit(RAM[PSW], CY) & 
		      (~GetBit(RAM[((IR & 0xF8) < 128) ? 
		      (((IR & 0xF8)>>3)+32) : 
		      (128 + (IR & 0xF8))], 
		      (IR & 0x07)) & 0x01)) != 0x00 ) {
		      SetBit(RAM[PSW], CY);
		      }
		      else {
		      ClearBit(RAM[PSW], CY);
		      }	*/	  
		    cycleCount += 24;
		    break;
		
		    //(A) <- (A) | (Rn)
		case ORL1:
		
		    #ifdef DEBUG
			os << "ORL 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A | R" << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[ACC] |= RAM[GetRegisterBank()+regNum];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) | (direct)		  
		case ORL2:
		
		    #ifdef DEBUG
			os << "ORL 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A | RAM(" << (unsigned int)(unsigned char)ROM[PC] << ")" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] |= RAM[IR < 128 ? IR : (IR+128)];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) | ((Ri))
		case ORL3:
		
		    #ifdef DEBUG
			os << "ORL 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A | RAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[ACC] |= RAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) | (#data)
		case ORL4:
		
		    #ifdef DEBUG
			os << "ORL 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A | "; PrintHex(ROM[PC], &os); os << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] |= (char)IR;
		    cycleCount += 12;
		    break;
		
		    // (direct) <- (direct) | (A)
		case ORL5:
		
		    #ifdef DEBUG
			os << "ORL 5" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " | A" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)] |= RAM[ACC];
		    cycleCount += 12;
		    break;
		
		    //(direct) <- (direct) | (#data)
		case ORL6:
		
		    #ifdef DEBUG
			os << "ORL 6" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " | "; 
			PrintHex(ROM[PC+1], &os); os << endl;
		    #endif
		    #endif
		    directAddr = ROM[PC++];
		    IR = ROM[PC++];
		    RAM[directAddr < 128 ? directAddr : (directAddr+128)]
			|= (char)IR;
		    cycleCount += 24;
		    break;
		
		    // (C) <- (C) | (bit)
		case ORL7:
		
		    #ifdef DEBUG
			os << "ORL 7" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- C | RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07)) ) {
			SetBit(RAM[PSW], CY);
		    }
		    /*if( (GetBit(RAM[PSW], CY) | 
		      GetBit(RAM[((IR & 0xF8) < 128) ? 
		      (((IR & 0xF8)>>3)+32) : 
		      (128 + (IR & 0xF8))], (IR & 0x07))) != 0x00 ) {
		      SetBit(RAM[PSW], CY);
		      }
		      else {
		      ClearBit(RAM[PSW], CY);
		      }*/
		    cycleCount += 24;
		    break;
		
		    // (C) <- (C) | /(bit)		  
		case ORL8:
		
		    #ifdef DEBUG
			os << "ORL 8" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- C | !RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( !GetBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07)) ) {
			SetBit(RAM[PSW], CY);
		    }
		    /*if( (GetBit(RAM[PSW], CY) | 
		      (~GetBit(RAM[((IR & 0xF8) < 128) ? 
		      (((IR & 0xF8)>>3)+32) : 
		      (128 + (IR & 0xF8))], 
		      (IR & 0x07)) & 0x01)) != 0x00 ) {
		      SetBit(RAM[PSW], CY);
		      }
		      else {
		      ClearBit(RAM[PSW], CY);
		      }*/		  
		    cycleCount += 24;
		    break;
		
		    //(A) <- (A) ^ (Rn)
		case XRL1:
		
		    #ifdef DEBUG
			os << "XRL 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A ^ R" << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[ACC] ^= RAM[GetRegisterBank()+regNum];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) ^ (direct)		  
		case XRL2:
		
		    #ifdef DEBUG
			os << "XRL 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A ^ RAM(" << (unsigned int)(unsigned char)ROM[PC] << ")" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] ^= RAM[IR < 128 ? IR : (IR+128)];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) ^ ((Ri))
		case XRL3:
		
		    #ifdef DEBUG
			os << "XRL 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A ^ RAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[ACC] ^= RAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 12;
		    break;
		
		    //(A) <- (A) ^ (#data)
		case XRL4:
		
		    #ifdef DEBUG
			os << "XRL 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A ^ "; PrintHex(ROM[PC], &os); os << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] ^= (char)IR;
		    cycleCount += 12;
		    break;
		
		    // (direct) <- (direct) ^ (A)
		case XRL5:
		
		    #ifdef DEBUG
			os << "XRL 5" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " ^ A" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)] ^= RAM[ACC];
		    cycleCount += 12;
		    break;
		
		    //(direct) <- (direct) ^ (#data)
		case XRL6:
		
		    #ifdef DEBUG
			os << "XRL 6" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") <- RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << " ^ "; 
			PrintHex(ROM[PC+1], &os); os << endl;
		    #endif
		    #endif
		    directAddr = ROM[PC++];
		    IR = ROM[PC++];
		    RAM[directAddr < 128 ? directAddr : (directAddr+128)] 
			^= (char)IR;
		    cycleCount += 24;
		    break;
		
		    // CLR (A)
		case CLR1:
		
		    #ifdef DEBUG
			os << "CLR 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- 0x00" << endl;
		    #endif
		    #endif
		    RAM[ACC] = 0x00;
		    cycleCount += 12;
		    break;
		
		    // CLR (C)
		case CLR2:
		
		    #ifdef DEBUG
			os << "CLR 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- 0" << endl;
		    #endif
		    #endif
		    ClearBit(RAM[PSW], CY);
		    cycleCount += 12;
		    break;
		
		    // CLR (bit)
		case CLR3:
		
		    #ifdef DEBUG
			os << "CLR 3" << endl;
		    #endif
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << " <- 0" << endl;
		    #endif
		    IR = ROM[PC++];
		    ClearBit(RAM[((IR & 0xF8) < 128) ? 
				(((IR & 0xF8)>>3)+32) : 
				(128 + (IR & 0xF8))], (IR & 0x07));
		    cycleCount += 12;
		    break;
		
		    // SETB (C)
		case SETB1:
		
		    #ifdef DEBUG
			os << "SETB 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- 1" << endl;
		    #endif
		    #endif
		    SetBit(RAM[PSW], CY);
		    cycleCount += 12;
		    break;
		
		    // SETB (bit)
		case SETB2:
		
		    #ifdef DEBUG
			os << "SETB 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << " <- 1" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    SetBit(RAM[((IR & 0xF8) < 128) ? 
			      (((IR & 0xF8)>>3)+32) : 
			      (128 + (IR & 0xF8))], (IR & 0x07));
		    cycleCount += 12;
		    break;
		
		    // CPL (A)
		case CPL1:
		
		    #ifdef DEBUG
			os << "CPL 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- ~A" << endl;
		    #endif
		    #endif
		    RAM[ACC] = ~RAM[ACC];
		    cycleCount += 12;
		    break;
		
		    // CPL (C)
		case CPL2:
		
		    #ifdef DEBUG
			os << "CPL 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- ~C" << endl;
		    #endif
		    #endif
		    if( GetBit(RAM[PSW], CY) == 0x01 ) {
			ClearBit(RAM[PSW], CY);
		    }
		    else {
			SetBit(RAM[PSW], CY);
		    }
		    cycleCount += 12;
		    break;
		
		    // CPL (bit)
		case CPL3:
		
		    #ifdef DEBUG
			os << "CPL 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (int)(((ROM[PC] & 0xF8) < 128) ? 
			    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << " <- ~RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07)) == 0x01 ) {
			ClearBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07));
		    }
		    else {
			SetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07));
		    }
		    cycleCount += 12;
		    break;
		    
		    // Rotate Left (A)
		case RL:
		    
		    #ifdef DEBUG
			os << "RL" << endl;
		    #ifdef DETAIL
			os << "\t" << "RL A" << endl;
		    #endif
		    #endif
		    rotateBit = GetBit(RAM[ACC], 7);
		    RAM[ACC] = (unsigned char)RAM[ACC] << 1;
		    if( rotateBit == 0x01 ) {
			SetBit(RAM[ACC], 0);
		    }
		    else {
			ClearBit(RAM[ACC], 0);
		    }
		    cycleCount += 12;
		    break;
		    
		    // Rotate Left Thru Carry(A)
		case RLC:
		   
		    #ifdef DEBUG
			os << "RLC" << endl;
		    #ifdef DETAIL
			os << "\t" << "RLC A" << endl;
		    #endif
		    #endif
		    rotateBit = GetBit(RAM[ACC], 7);
		    RAM[ACC] = (unsigned char)RAM[ACC] << 1;		    
		    if( GetBit(RAM[PSW], CY) == 0x01 ) {
			SetBit(RAM[ACC], 0);
		    }
		    else {
			ClearBit(RAM[ACC], 0);
		    }
		    if( rotateBit == 0x01 ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 12;
		    break;
		    
		    // Rotate Right (A)
		case RR:
		    
		    #ifdef DEBUG
			os << "RR" << endl;
		    #ifdef DETAIL
			os << "\t" << "RR A" << endl;
		    #endif
		    #endif
		    rotateBit = GetBit(RAM[ACC], 0);
		    RAM[ACC] = (unsigned char)RAM[ACC] >> 1;
		    if( rotateBit == 0x01 ) {
			SetBit(RAM[ACC], 7);
		    }
		    else {
			ClearBit(RAM[ACC], 7);
		    }
		    
		    cycleCount += 12;
		    break;
		    
		    // Rotate Right Thru Carry(A)
		case RRC:
		    
		    #ifdef DEBUG
			os << "RRC" << endl;
		    #ifdef DETAIL
			os << "\t" << "RRC A" << endl;
		    #endif
		    #endif
		    rotateBit = GetBit(RAM[ACC], 0);
		    RAM[ACC] = (unsigned char)RAM[ACC] >> 1;
		    if( GetBit(RAM[PSW], CY) == 0x01 ) {
			SetBit(RAM[ACC], 7);
		    }
		    else {
			ClearBit(RAM[ACC], 7);
		    }
		    if( rotateBit == 0x01 ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 12;
		    break;
		    
		    // Swap nibbles(A)
		case SWAP:
		    
		    #ifdef DEBUG
			os << "SWAP" << endl;
		    #ifdef DETAIL
			os << "\t" << "A.(3-0) <-> A(7-4)" << endl;
		    #endif
		    #endif
		    lowerNibble = RAM[ACC] & 0x0F;
		    RAM[ACC] = (RAM[ACC] >> 4) & 0x0F;
		    RAM[ACC] |= ((lowerNibble << 4) & 0xF0);
		    cycleCount += 12;
		    break;
		    
		    // XCH (A), (Rn)
		case XCH1:
		    
		    #ifdef DEBUG
			os << "XCH 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <-> R" << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    temp = RAM[ACC];
		    RAM[ACC] = RAM[GetRegisterBank()+regNum];
		    RAM[GetRegisterBank()+regNum] = temp;
		    cycleCount += 12;
		    break;
		    
		    // XCH (A), (direct)
		case XCH2:
		    
		    #ifdef DEBUG
			os << "XCH 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <-> RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    temp = RAM[ACC];
		    RAM[ACC] = RAM[IR < 128 ? IR : (IR+128)];
		    RAM[IR < 128 ? IR : (IR+128)] = temp;
		    cycleCount += 12;
		    break;
		    
		    // XCH (A), ((Ri))
		case XCH3:
		    
		    #ifdef DEBUG
			os << "XCH 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <-> RAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    temp = RAM[ACC];
		    RAM[ACC] = RAM[RAM[GetRegisterBank()+regNum]];
		    RAM[RAM[GetRegisterBank()+regNum]] = temp;
		    cycleCount += 12;
		    break;
		    
		    // XCHD (A), ((Ri))
		case XCHD:
		    
		    #ifdef DEBUG
			os << "XCHD" << endl;
		    #ifdef DETAIL
			os << "\t" << "A.(3-0) <-> RAM(R" << (int)(IR & 0x01) << ").(3-0)" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    temp = RAM[ACC] & 0x0F;
		    RAM[ACC] = (RAM[ACC] & 0xF0) | 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x0F);
		    RAM[RAM[GetRegisterBank()+regNum]] = 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0xF0 ) | temp;
		    cycleCount += 12;
		    break;
		    
		    // LCALL addr16
		case LCALL:
		    
		    ((unsigned char*)&jumpAddr)[1] = ROM[PC++];
		    ((unsigned char*)&jumpAddr)[0] = ROM[PC++];
		    #ifdef DEBUG
			os << "LCALL" << endl;
		    #ifdef DETAIL
			os << "\t" << "LCALL " << jumpAddr << endl;
		    #endif
		    #endif
		    RAM[SP] = (unsigned char)RAM[SP] + 1;
		    RAM[(unsigned char)RAM[SP]]  = ((unsigned char*)&PC)[0];
		    RAM[SP] = (unsigned char)RAM[SP] + 1;
		    RAM[(unsigned char)RAM[SP]]  = ((unsigned char*)&PC)[1];
		    PC = jumpAddr;
		    cycleCount += 24;
		    break;
		    
		    // ACALL addr11
		case ACALL:
		  
		    ((unsigned char*)&jumpAddr)[0] = ROM[PC++];
		    ((unsigned char*)&jumpAddr)[1] = (PC & 0xF800) | ((IR & 0xE0) >> 5);
		    
		    //((unsigned char*)&jumpAddr)[1] = ((IR & 0xE0) >> 1);
		    //((unsigned char*)&jumpAddr)[0] = ROM[PC++];
		    #ifdef DEBUG
			os << "ACALL" << endl;
		    #ifdef DETAIL
			os << "\t" << "ACALL " << jumpAddr << endl;
		    #endif
		    #endif
		    RAM[SP] = (unsigned char)RAM[SP] + 1;
		    RAM[(unsigned char)RAM[SP]]  = ((unsigned char*)&PC)[0];
		    RAM[SP] = (unsigned char)RAM[SP] + 1;
		    RAM[(unsigned char)RAM[SP]]  = ((unsigned char*)&PC)[1];
		    PC = jumpAddr;
		    cycleCount += 24;
		    break;
		    
		    // DA (A)
		case DA:
		    
		    #ifdef DEBUG
			os << "DA" << endl;
		    #ifdef DETAIL
			os << "\t" << "DA A" << endl;
		    #endif
		    #endif
		    if( (RAM[ACC] & 0x0F) > 9 || 
			GetBit(RAM[PSW], AC) == 0x01 ) {
			tempAdd = RAM[ACC] + 0x06;
			RAM[ACC] = (char)tempAdd;
			if( ((unsigned char*)&tempAdd)[1] != 0 ) {
			    SetBit(RAM[PSW], CY);
			}
		    }
		    if( ((RAM[ACC] & 0xF0) >> 4) > 9 || 
			GetBit(RAM[PSW], CY) == 0x01 ) {
			tempAdd = RAM[ACC] + 0x60;
			RAM[ACC] = (char)tempAdd;
			if( ((unsigned char*)&tempAdd)[1] != 0 ) {
			    SetBit(RAM[PSW], CY);
			}
		    }
		    cycleCount += 12;
		    break;
		    
		    // CJNE (A), (direct), (rel)
		case CJNE1:
		    
		    #ifdef DEBUG
			os << "CJNE 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( A != RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") ) JMP " << (PC+(char)ROM[PC+1]+2) << endl
			   << "\t" << "if( A < RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") ) C <- 1" << endl
			   << "\t" << "else C <- 0" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    directAddr = IR;
		    if( RAM[ACC] != RAM[IR < 128 ? IR : (IR+128)] ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    if( (unsigned char)RAM[ACC] < 
			(unsigned char)RAM[directAddr < 128 ? directAddr: 
					  (directAddr+128)] ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 24;
		    break;
		    
		    // CJNE (A), (#data), (rel)
		case CJNE2:
		   
		    #ifdef DEBUG
			os << "CJNE 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( A != ";
			PrintHex(ROM[PC], &os);
			os << " ) JMP " << (PC+(char)ROM[PC+1]+2) << endl
			   << "\t" << "if( A < ";
			PrintHex(ROM[PC], &os);
			os << " ) C <- 1" << endl
			   << "\t" << "else C <- 0" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    directAddr = IR;
		    if( RAM[ACC] != (char)IR ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    if( (unsigned char)RAM[ACC] < (unsigned char)directAddr ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 24;
		    break;
		    
		    // CJNE (Rn), (#data), (rel)
		case CJNE3:
		    
		    #ifdef DEBUG
			os << "CJNE 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( R" << (int)(IR & 0x07) << " != ";
			PrintHex(ROM[PC], &os);
			os << " ) JMP " << (PC+(char)ROM[PC+1]+2) << endl
			   << "\t" << "if( R" << (int)(IR & 0x07) << " < ";
			PrintHex(ROM[PC], &os);
			os << " ) C <- 1" << endl
			   << "\t" << "else C <- 0" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    IR = ROM[PC++];
		    directAddr = IR;
		    if( RAM[GetRegisterBank()+regNum] != (char)IR ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    if( (unsigned char)RAM[GetRegisterBank()+regNum] < 
			(unsigned char)directAddr ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 24;
		    break;
		    
		    // CJNE ((Ri)), (#data), (rel)
		case CJNE4:
		    
		    #ifdef DEBUG
			os << "CJNE 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( RAM(R" << (int)(IR & 0x01) << ") != ";
			PrintHex(ROM[PC], &os);
			os << " ) JMP " << (PC+(char)ROM[PC+1]+2) << endl
			   << "\t" << "if( RAM(R" << (int)(IR & 0x01) << ") < ";
			PrintHex(ROM[PC], &os);
			os << " ) C <- 1" << endl
			   << "\t" << "else C <- 0" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    IR = ROM[PC++];
		    directAddr = IR;
		    if( RAM[RAM[GetRegisterBank()+regNum]] != (char)IR ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    
		    if( (unsigned char)RAM[RAM[GetRegisterBank()+regNum]] < 
			(unsigned char)directAddr ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 24;
		    break;
		    
		    // DEC (A)
		case DEC1:
		    
		    #ifdef DEBUG
			os << "DEC 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A--" << endl;
		    #endif
		    #endif
		    RAM[ACC]--;
		    cycleCount += 12;
		    break;
		    
		    // DEC (Rn)
		case DEC2:
		  
		    #ifdef DEBUG
			os << "DEC 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << "--" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[GetRegisterBank()+regNum]--;
		    cycleCount += 12;
		    break;
		    
		    // DEC (direct)
		case DEC3:
		    
		    #ifdef DEBUG
			os << "DEC 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] << ")--" << endl; 
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)]--;
		    cycleCount += 12;
		    break;
		    
		    // DEC ((Ri))
		case DEC4:
		    
		    #ifdef DEBUG
			os << "DEC 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(R" << (int)(IR & 0x01) << ")--" << endl; 
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[RAM[GetRegisterBank()+regNum]]--;
		    cycleCount += 12;
		    break;
		    
		    // DIV (A)/(B)
		case DIV:
		    
		    #ifdef DEBUG
			os << "DIV" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A / B" << endl;
			os << "\t" << "B <- A % B" << endl;
		    #endif
		    #endif
		    if( RAM[B] == 0x00 ) {
			SetBit(RAM[PSW], OV);
		    }
		    else {
			ClearBit(RAM[PSW], OV);
			tempACC = RAM[ACC];
			RAM[ACC] = tempACC/RAM[B];
			RAM[B] = tempACC%RAM[B];
		    }		    
		    ClearBit(RAM[PSW], CY);
		    cycleCount += 48;
		    break;
		    
		    // INC (A)
		case INC1:
		    
		    #ifdef DEBUG
			os << "INC 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A++" << endl;
		    #endif
		    #endif
		    RAM[ACC]++;
		    cycleCount += 12;
		    break;
		    
		    // INC (Rn)
		case INC2:
		    
		    #ifdef DEBUG
			os << "INC 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << "++" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[GetRegisterBank()+regNum]++;
		    cycleCount += 12;
		    break;
		    
		    // INC (direct)
		case INC3:
		    
		    #ifdef DEBUG
			os << "INC 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)ROM[PC] << ")++" << endl; 
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)]++;
		    cycleCount += 12;
		    break;
		    
		    // INC ((Ri))
		case INC4:
		    
		    #ifdef DEBUG
			os << "INC 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(R" << (int)(IR & 0x01) << ")++" << endl; 
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[RAM[GetRegisterBank()+regNum]]++;
		    cycleCount += 12;
		    break;
		    
		    // INC (DPTR)
		case INC5:
		    
		    #ifdef DEBUG
			os << "INC 5" << endl;
		    #ifdef DETAIL
			os << "\t" << "DPTR++" << endl;
		    #endif
		    #endif
		    ((unsigned char*)&tempDPTR)[1] = RAM[DPH];
		    ((unsigned char*)&tempDPTR)[0] = RAM[DPL];
		    tempDPTR++;
		    RAM[DPH] = ((unsigned char*)&tempDPTR)[1];
		    RAM[DPL] = ((unsigned char*)&tempDPTR)[0];
		    cycleCount += 24;
		    break;
		    
		    // NOP
		case NOP:
		    
		    #ifdef DEBUG
			os << "NOP" << endl;
		    #ifdef DETAIL
			os << "\t" << "nothing" << endl;
		    #endif
		    #endif
		    // no code
		    cycleCount += 12;
		    break;
		    
		    // DJNZ (Rn), (rel)  
		case DJNZ1:
		    
		    #ifdef DEBUG
			os << "DJNZ 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << "--" << endl;
			os << "\t" << "if( R" <<  (int)(IR & 0x07)
			   << " != 0 ) JMP " 
			   << (PC+(char)ROM[PC]+1) << endl;			
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    
		    RAM[GetRegisterBank()+regNum] = 
			(unsigned char)RAM[GetRegisterBank()+regNum] - 1;
		    if( RAM[GetRegisterBank()+regNum] != 0x00 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // DJNZ (direct), (rel)  
		case DJNZ2:
		    
		    #ifdef DEBUG
			os << "DJNZ 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" 
			   << (unsigned int)(unsigned char)ROM[PC] << ")--" << endl; 
			os << "\t" << "if( RAM(" << (unsigned int)(unsigned char)ROM[PC] 
			   << ") != 0 ) JMP " 
			   << (PC+(char)ROM[PC+1]+2) << endl;			
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    IR = ROM[PC++];
		    RAM[IR < 128 ? IR : (IR+128)] = 
			(unsigned char)RAM[IR < 128 ? IR : (IR+128)] - 1;
		    if( RAM[IR < 128 ? IR : (IR+128)] != 0x00 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // MUL (A), (B)
		case MUL:
		    
		    #ifdef DEBUG
			os << "MUL" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- (A * B).(7-0)" << endl;
			os << "\t" << "B <- (A * B).(15-8)" << endl;
		    #endif
		    #endif
		    tempProduct = (unsigned char)RAM[ACC] * 
			(unsigned char)RAM[B];
		    if( tempProduct > 255 ) {
			SetBit(RAM[PSW], OV);
		    }
		    else {
			ClearBit(RAM[PSW], OV);
		    }
		    ClearBit(RAM[PSW], CY);
		    RAM[ACC] = ((unsigned char*)&tempProduct)[0];
		    RAM[B] = ((unsigned char*)&tempProduct)[1];
		    cycleCount += 48;
		    break;
		    
		    // POP (direct)
		case POP:
		    
		    #ifdef DEBUG
			os << "POP" << endl;
		    #ifdef DETAIL
			os << "\t" << "POP RAM(" 
			   << (unsigned int)(unsigned char)(ROM[PC]) << ")"
			   << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    popData = RAM[(unsigned char)RAM[SP]];
		    RAM[SP] = (unsigned char)RAM[SP] - 1;
		    RAM[IR < 128 ? IR : (IR+128)] = popData;
		    cycleCount += 24;
		    break;
		    
		    // PUSH (direct)
		case PUSH:
		    
		    #ifdef DEBUG
			os << "PUSH" << endl;
		    #ifdef DETAIL
			os << "\t" << "PUSH RAM(" 
			   << (unsigned int)(unsigned char)(ROM[PC]) << ")"
			   << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[SP] = (unsigned char)RAM[SP] + 1;
		    RAM[(unsigned char)RAM[SP]] = RAM[IR < 128 ? IR : (IR+128)];
		    cycleCount += 24;
		    break;
		    
		    // JB (bit), (rel)
		case JB:
		    
		    #ifdef DEBUG
			os << "JB" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) 
			   << " ) JMP " << (PC+(char)ROM[PC+1]+2)
			   << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07)) == 0x01 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JBC (bit), (rel)
		case JBC:
		    
		    #ifdef DEBUG
			os << "JBC" << endl;
		    #ifdef DETAIL
			os << "if( RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) 
			   << " )" << endl << "\t" << "{" << endl
			   << "\t\t" << "JMP " << (PC+(char)ROM[PC+1]+2)
			   << "\t\t" << "RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << " <- 0"
			   << "\t" << "}" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07)) == 0x01 ) {
			ClearBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07));
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JC (rel)
		case JC:
		    
		    #ifdef DEBUG
			os << "JC" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( C ) JMP " 
			   << (PC+(char)ROM[PC]+1) << endl;
		    #endif
		    #endif
		    if( GetBit(RAM[PSW], CY) == 0x01 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JMP @A+DPTR
		case JMP:
		   
		    ((unsigned char*)&tempDPTR)[1] = RAM[DPH];
		    ((unsigned char*)&tempDPTR)[0] = RAM[DPL];
		    #ifdef DEBUG
			os << "JMP" << endl;
		    #ifdef DETAIL
			os << "JMP A + " << tempDPTR << endl;
		    #endif
		    #endif
		    tempDPTR += (unsigned char)RAM[ACC];
		    PC = (unsigned short)tempDPTR;
		    cycleCount += 24;
		    break;
		    
		    // JNB (bit), (rel)
		case JNB:
		    
		    #ifdef DEBUG
			os << "JNB" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( !RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) 
			   << " ) JMP " << (PC+(char)ROM[PC+1]+2)
			   << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07)) != 0x01 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JNC (rel)
		case JNC:
		    
		    #ifdef DEBUG
			os << "JNC" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( !C ) JMP " 
			   << (PC+(char)ROM[PC]+1) << endl;			
		    #endif
		    #endif
		    if( GetBit(RAM[PSW], CY) == 0x00 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JNZ (rel)
		case JNZ:
		    
		    #ifdef DEBUG
			os << "JNZ" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( A != 0 ) JMP " 
			   << (PC+(char)ROM[PC]+1) << endl;			
		    #endif
		    #endif
		    if( RAM[ACC] != 0x00 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // JZ (rel)
		case JZ:
		    
		    #ifdef DEBUG
			os << "JZ" << endl;
		    #ifdef DETAIL
			os << "\t" << "if( A == 0 ) JMP " 
			   << (PC+(char)ROM[PC]+1) << endl;
		    #endif
		    #endif
		    if( RAM[ACC] == 0x00 ) {
			IR = ROM[PC++];
			PC += (char)IR;
		    }
		    else {
			PC++;
		    }
		    cycleCount += 24;
		    break;
		    
		    // AJMP addr11
		case AJMP:
		    
		  ((unsigned char*)&jumpAddr)[0] = ROM[PC++];
		  ((unsigned char*)&jumpAddr)[1] = (PC & 0xF800) | ((IR & 0xE0) >> 5);
		    #ifdef DEBUG
			os << "AJMP" << endl;
		    #ifdef DETAIL
			os << "\t" << "AJMP " << jumpAddr << endl;
		    #endif
		    #endif
		    PC = jumpAddr;
		    cycleCount += 24;
		    break;
		    
		    // LJMP addr16
		case LJMP:
		    
		    ((unsigned char*)&jumpAddr)[1] = ROM[PC++];
		    ((unsigned char*)&jumpAddr)[0] = ROM[PC++];
		    #ifdef DEBUG
			os << "LJMP " << endl;
		    #ifdef DETAIL
			os << "\t" << "LJMP" << jumpAddr << endl;
		    #endif
		    #endif
		    PC = jumpAddr;
		    cycleCount += 24;
		    break;
		    
		    // SJMP (rel)
		case SJMP:
		    
		    #ifdef DEBUG
			os << "SJMP" << endl;
		    #ifdef DETAIL
			os << "\t" << "SJMP " 
			   << (PC+(char)ROM[PC]+1) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    PC += (char)IR;
		    cycleCount += 24;
		    break;
		    
		    // A <- Rn
		case MOV1:
		    
		    #ifdef DEBUG
			os << "MOV 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- R" << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[ACC] = RAM[GetRegisterBank()+regNum];
		    cycleCount += 12;
		    break;
		    
		    // A <- direct
		case MOV2:
		    
		    #ifdef DEBUG
			os << "MOV 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] = RAM[IR < 128 ? IR : (IR + 128)];
		    cycleCount += 12;
		    break;
		    
		    // A <- @Ri
		case MOV3:
		    
		    #ifdef DEBUG
			os << "MOV 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- RAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[ACC] = RAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 12;
		    break;
		    
		    // A <- #data
		case MOV4:
		    
		    #ifdef DEBUG
			os << "MOV 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- ";
			PrintHex(ROM[PC], &os);
			os << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[ACC] = (char)IR;
		    cycleCount += 12;
		    break;
		    
		    // Rn <- A
		case MOV5:
		    
		    #ifdef DEBUG
			os << "MOV 5" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << " <- A" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    RAM[GetRegisterBank()+regNum] = RAM[ACC];
		    cycleCount += 12;
		    break;
		    
		    // Rn <- direct
		case MOV6:
		    
		    #ifdef DEBUG
			os << "MOV 6" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << " <- RAM(" 
			   << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    IR = ROM[PC++];
		    RAM[GetRegisterBank()+regNum] = 
			RAM[ (IR < 128) ? IR : (IR + 128)]; 
		    cycleCount += 24;
		    break;
		    
		    // Rn <- #data
		case MOV7:
		    
		    #ifdef DEBUG
			os << "MOV 7" << endl;
		    #ifdef DETAIL
			os << "\t" << "R" << (int)(IR & 0x07) << " <- ";
			PrintHex(ROM[PC], &os);
			os << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    IR = ROM[PC++];
		    RAM[GetRegisterBank()+regNum] = (char)IR;
		    cycleCount += 12;
		    break;

		    // direct <- A
		case MOV8:
		    
		    #ifdef DEBUG
			os << "MOV 8" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") <- A" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    RAM[(IR < 128) ? IR : (IR+128)] = RAM[ACC];
		    cycleCount += 12;
		    break;
		    
		    // direct <- Rn
		case MOV9:
		    
		    #ifdef DEBUG
			os << "MOV 9" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") <- R" 
			   << (int)(IR & 0x07) << endl;
		    #endif
		    #endif
		    regNum = IR & 0x07;
		    IR = ROM[PC++];
		    RAM[(IR<128) ? IR : (IR+128)] = 
			RAM[GetRegisterBank()+regNum];
		    cycleCount += 24;
		    break;
		    
		    // direct <- direct
		case MOV10:
		    
		    #ifdef DEBUG
			os << "MOV 10" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(ROM[PC+1]) << ") <- RAM(" 
			   << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
		    #endif
		    #endif
		    directAddr = ROM[PC++];
		    IR = ROM[PC++];
		    RAM[(IR<128) ? IR : (IR+128)] = 
			RAM[(directAddr < 128) ? directAddr : (directAddr+128)];
		    cycleCount += 24;
		    break;
		    
		    // direct <- ((Ri))
		case MOV11:
		    
		    #ifdef DEBUG
			os << "MOV 11" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") <- RAM(R" 
			   << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    IR = ROM[PC++];
		    RAM[(IR <128) ? IR : (IR+128)] = 
			RAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 24;
		    break;
		    
		    // direct <- #data
		case MOV12:
		    
		    #ifdef DEBUG
			os << "MOV 12" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") <- ";
			PrintHex(ROM[PC+1], &os);
			os << endl;
		    #endif
		    #endif
		    directAddr = ROM[PC++];
		    IR = ROM[PC++];
		    RAM[(directAddr<128) ? directAddr : (directAddr+128)] = 
			(char)IR;
		    cycleCount += 24;
		    break;
		    
		    // Ri <- A
		case MOV13:
		    
		    #ifdef DEBUG
			os << "MOV 13" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(R" << (int)(IR & 0x01) << ") <- A" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[RAM[GetRegisterBank()+regNum]] = RAM[ACC];
		    cycleCount += 12;
		    break;
		    
		    // Ri <- direct
		case MOV14:
		    
		    #ifdef DEBUG
			os << "MOV 14" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(R" << (int)(IR & 0x01) << ") <- RAM(" 
			   << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    IR = ROM[PC++];
		    RAM[RAM[GetRegisterBank()+regNum]] = 
			RAM[(IR<128) ? IR : (IR+128)];
		    cycleCount += 24;
		    break;
		    
		    // @Ri, #data
		case MOV15:
		    
		    #ifdef DEBUG
			os << "MOV 15" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(R" << (int)(IR & 0x01) << ") <- ";
			PrintHex(ROM[PC], &os);
			os << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    IR = ROM[PC++];;
		    RAM[RAM[GetRegisterBank()+regNum]] = (char)IR;
		    cycleCount += 12;
		    break;
		    
		    // MOV (C), bit
		case MOV16:
		    
		    #ifdef DEBUG
			os << "MOV 16" << endl;
		    #ifdef DETAIL
			os << "\t" << "C <- RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07) << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07)) == 0x01 ) {
			SetBit(RAM[PSW], CY);
		    }
		    else {
			ClearBit(RAM[PSW], CY);
		    }
		    cycleCount += 12;
		    break;
		    
		    // MOV bit, (C)
		case MOV17:
		    
		    #ifdef DEBUG
			os << "MOV 17" << endl;
		    #ifdef DETAIL
			os << "\t" << "RAM(" 
			   << (int)(((ROM[PC] & 0xF8) < 128) ? 
				    (((ROM[PC] & 0xF8)>>3)+32) : (128 + (ROM[PC] & 0xF8)))
			   << ")." << (int)(ROM[PC] & 0x07)
			   << " <- C" << endl;
		    #endif
		    #endif
		    IR = ROM[PC++];
		    if( GetBit(RAM[PSW], CY) == 0x01 ) {
			SetBit(RAM[((IR & 0xF8) < 128) ? 
				  (((IR & 0xF8)>>3)+32) : 
				  (128 + (IR & 0xF8))], (IR & 0x07));
		    }
		    else {
			ClearBit(RAM[((IR & 0xF8) < 128) ? 
				    (((IR & 0xF8)>>3)+32) : 
				    (128 + (IR & 0xF8))], (IR & 0x07));
		    }
		    cycleCount += 24;
		    break;
		    
		    // MOV DPTR, data16
		case MOV18:
		    
		    #ifdef DEBUG
			os << "MOV 18" << endl;
		    #ifdef DETAIL
			((unsigned char*)&tempDPTR)[1] = ROM[PC];
			((unsigned char*)&tempDPTR)[0] = ROM[PC+1];
			os << "\t" << "DPTR <- " << tempDPTR << endl;
		    #endif
		    #endif
		    RAM[DPH] = ROM[PC++];
		    RAM[DPL] = ROM[PC++];
		    cycleCount += 24;
		    break;
		    
		    // MOVC (A), @A+DPTR
		case MOVC1:
		    
		    #ifdef DEBUG
			os << "MOVC 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- ROM(A+DPTR)" << endl;
		    #endif
		    #endif
		    ((unsigned char*)&tempDPTR)[1] = RAM[DPH];
		    ((unsigned char*)&tempDPTR)[0] = RAM[DPL];
		    RAM[ACC] = ROM[(unsigned short)(unsigned char)RAM[ACC]+tempDPTR];
		    cycleCount += 24;
		    break;
		    
		    // MOVC (A), @A+PC
		case MOVC2:
		    
                    #ifdef DEBUG
		        os << "MOVC 2" << endl;
                    #ifdef DETAIL
		        os << "\t" << "A <- ROM(A+PC)" << endl;
                    #endif
                    #endif
		    RAM[ACC] = ROM[(unsigned char)RAM[ACC]+PC];
		    cycleCount += 24;
		    break;
			
		    // MOVX (A), @RI
		case MOVX1:		    
		    #ifdef DEBUG
			os << "MOVX 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- XRAM(R" << (int)(IR & 0x01) << ")" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    RAM[ACC] = XRAM[RAM[GetRegisterBank()+regNum]];
		    cycleCount += 24;
		    break;

		    // MOVX (A), @DPTR
		case MOVX2:
		    ((unsigned char*)&tempDPTR)[1] = RAM[DPH];
		    ((unsigned char*)&tempDPTR)[0] = RAM[DPL];
		    #ifdef DEBUG
			os << "MOVX 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- XRAM(" << tempDPTR << ")" << endl;
		    #endif
		    #endif
		    RAM[ACC] = XRAM[tempDPTR];
		    cycleCount += 24;
		    break;
		    
		    // MOVX @RI, A
		case MOVX3:
		    #ifdef DEBUG
			os << "MOVX 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "XRAM(R" << (int)(IR & 0x01) << ") <- A" << endl;
		    #endif
		    #endif
		    regNum = IR & 0x01;
		    XRAM[RAM[GetRegisterBank()+regNum]] = RAM[ACC];
		    cycleCount += 24;
		    break;
		  
		    // MOVX @DPTR, A
		case MOVX4:
		    ((unsigned char*)&tempDPTR)[1] = RAM[DPH];
		    ((unsigned char*)&tempDPTR)[0] = RAM[DPL];
		    #ifdef DEBUG
			os << "MOVX 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "XRAM(" << tempDPTR << ") <- A" << endl;
		    #endif
		    #endif
		    XRAM[tempDPTR] = RAM[ACC];
		    cycleCount += 24;
		    break;

		    // ADD A, (Rn)
		case ADD1:
		    
		    #ifdef DEBUG
			os << "ADD 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + R" << (int)(IR & 0x07) << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    regNum = IR & 0x07;

		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[GetRegisterBank()+regNum], &os); 
			os << endl;
                    #endif
                    #endif

		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[GetRegisterBank()+regNum] & 0x0F);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += ((RAM[ACC] & 0x70) + 
			(RAM[GetRegisterBank()+regNum] & 0x70));
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += ((RAM[ACC] & 0x80) + 
			(RAM[GetRegisterBank()+regNum] & 0x80));
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = tempAdd;
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;

		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif

		    break;
		    
		    // ADD A, (direct)
		case ADD2:
		    
		    #ifdef DEBUG
			os << "ADD 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ")" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    IR = ROM[PC++];

		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[IR <128 ? IR : (IR+128)], &os); 
			os << endl;
                    #endif
                    #endif

		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x0F);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += ((RAM[ACC] & 0x70) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x70));
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += ((RAM[ACC] & 0x80) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x80));
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = tempAdd;
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // ADD A, ((Ri))
		case ADD3:
		    
		    #ifdef DEBUG
			os << "ADD 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + RAM(R" << (int)(IR & 0x01) << ")" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    regNum = IR & 0x01;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[RAM[GetRegisterBank()+regNum]], &os); 
			os << endl;
                    #endif
                    #endif
		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x0F);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += ((RAM[ACC] & 0x70) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x70));
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += ((RAM[ACC] & 0x80) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x80));
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = tempAdd;
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // ADD A, (#data)
		case ADD4:
		    
		    #ifdef DEBUG
			os << "ADD 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + ";
			PrintHex(ROM[PC], &os);
			os << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    IR = ROM[PC++];
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex((char)IR, &os); 
			os << endl;
                    #endif
                    #endif
		    tempAdd = (RAM[ACC] & 0x0F) + ((char)IR & 0x0F);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += ((RAM[ACC] & 0x70) + ((char)IR & 0x70));
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += ((RAM[ACC] & 0x80) + ((char)IR & 0x80));
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = tempAdd;
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // ADDC A, (Rn)
		case ADDC1:
		    
		    #ifdef DEBUG
			os << "ADDC 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + R" << (int)(IR & 0x07) << " + C" << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    regNum = IR & 0x07;
		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[GetRegisterBank()+regNum] & 0x0F) + 
			(char)GetBit(RAM[PSW], CY);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += (RAM[ACC] & 0x70) + 
			(RAM[GetRegisterBank()+regNum] & 0x70);
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += (RAM[ACC] & 0x80) + 
			(RAM[GetRegisterBank()+regNum] & 0x80);
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = (unsigned char)(tempAdd & 0x00FF);
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    break;
		    
		    // ADDC A, (direct)
		case ADDC2:
		    
		    #ifdef DEBUG
			os << "ADDC 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") + C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    IR = ROM[PC++];
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[IR <128 ? IR : (IR+128)], &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x0F) +
			(char)GetBit(RAM[PSW], CY);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += (RAM[ACC] & 0x70) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x70);
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += (RAM[ACC] & 0x80) + 
			(RAM[IR <128 ? IR : (IR+128)] & 0x80);
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = (unsigned char)(tempAdd & 0x00FF);
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // ADDC A, ((Ri))
		case ADDC3:
		    
		    #ifdef DEBUG
			os << "ADDC 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + RAM(R" << (int)(IR & 0x01) << ") + C" << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    regNum = IR & 0x01;
		    tempAdd = (RAM[ACC] & 0x0F) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x0F) + 
			(char)GetBit(RAM[PSW], CY);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += (RAM[ACC] & 0x70) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x70);
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += (RAM[ACC] & 0x80) + 
			(RAM[RAM[GetRegisterBank()+regNum]] & 0x80);
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = (unsigned char)(tempAdd & 0x00FF);
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    break;
		    
		    // ADDC A, (#data)
		case ADDC4:
		    
		    #ifdef DEBUG
			os << "ADDC 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A + ";
			PrintHex(ROM[PC], &os);
			os << " + C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    carry3 = false;
		    carry6 = false;
		    carry7 = false;
		    IR = ROM[PC++];
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex((char)IR, &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    tempAdd = (RAM[ACC] & 0x0F) + 
			(IR & 0x0F) + GetBit(RAM[PSW], CY);
		    if( (tempAdd & 0x0010) == 0x0010 ) carry3 = true;
		    tempAdd += ((RAM[ACC] & 0x70) + ((char)IR & 0x70));
		    if( (tempAdd & 0x0080) == 0x0080 ) carry6 = true;
		    tempAdd += ((RAM[ACC] & 0x80) + ((char)IR & 0x80));
		    if( (tempAdd & 0x0100) == 0x0100 ) carry7 = true;
		    RAM[ACC] = (unsigned char)(tempAdd & 0x00FF);
		    if( carry3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( carry7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (carry6 && !carry7) || (!carry6 && carry7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A + B = ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << carry3 << " " << carry6 << " " << carry7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // SUBB (A), (Rn)
		case SUBB1:
		    
		    #ifdef DEBUG
			os << "SUBB 1" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A - R" << (int)(IR & 0x07) << " - C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    borrow3 = false;
		    borrow6 = false;
		    borrow7 = false;
		    regNum = IR & 0x07;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[GetRegisterBank()+regNum], &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    if( (unsigned char)(RAM[ACC] & 0x0F) <
			(unsigned char)((RAM[GetRegisterBank()+regNum] & 0x0F) +  
					(char)GetBit(RAM[PSW], CY)) ) {
			borrow3 = true;
		    } 
		    if( (unsigned char)(RAM[ACC] & 0x7F) <
			(unsigned char)((RAM[GetRegisterBank()+regNum] & 0x7F) + 
					(char)GetBit(RAM[PSW], CY)) ) {
			borrow6 = true;
		    }
		    if( (unsigned short)(unsigned char)RAM[ACC] <
			((unsigned short)(unsigned char)RAM[GetRegisterBank()+regNum] + 
			 (unsigned short)GetBit(RAM[PSW], CY)) ) {
			borrow7 = true;
		    }
	            RAM[ACC] = (unsigned short)(unsigned char)RAM[ACC] - 
			((unsigned short)(unsigned char)RAM[GetRegisterBank()+regNum] + 
			 (unsigned short)GetBit(RAM[PSW], CY));
		    if( borrow3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( borrow7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (borrow6 && !borrow7) || (!borrow6 && borrow7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    break;
		    
		    // SUBB (A), direct
		case SUBB2:
		    
		    #ifdef DEBUG
			os << "SUBB 2" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A - RAM(" << (unsigned int)(unsigned char)(ROM[PC]) << ") - C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    borrow3 = false;
		    borrow6 = false;
		    borrow7 = false;
		    IR = ROM[PC++];
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[IR <128 ? IR : (IR+128)], &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    if( (unsigned char)(RAM[ACC] & 0x0F) <
			(unsigned char)((RAM[IR <128 ? IR : (IR+128)] & 0x0F) + 
			 (char)GetBit(RAM[PSW], CY)) ) {
			borrow3 = true;
		    }
		    if( (unsigned char)(RAM[ACC] & 0x7F) <
			(unsigned char)((RAM[IR <128 ? IR : (IR+128)] & 0x7F) + 
			 (char)GetBit(RAM[PSW], CY)) ) {
			borrow6 = true;
		    }
		    if( (unsigned short)(unsigned char)RAM[ACC] <
			((unsigned short)(unsigned char)RAM[IR <128 ? IR : (IR+128)] + 
			 (unsigned short)GetBit(RAM[PSW], CY)) ) {
			borrow7 = true;
		    }
		    RAM[ACC] = (unsigned short)(unsigned char)RAM[ACC] - 
			((unsigned short)RAM[IR <128 ? IR : (IR+128)] + 
			 (unsigned short)GetBit(RAM[PSW], CY));
		    if( borrow3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( borrow7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (borrow6 && !borrow7) || (!borrow6 && borrow7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A - (B + C)= ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << borrow3 << " " << borrow6 << " " << borrow7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // SUBB (A), ((Ri))
		case SUBB3:
		    
		    #ifdef DEBUG
			os << "SUBB 3" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A - RAM(R" << (int)(IR & 0x01) << ") - C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    borrow3 = false;
		    borrow6 = false;
		    borrow7 = false;
		    regNum = IR & 0x01;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex(RAM[RAM[GetRegisterBank()+regNum]], &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    if( (unsigned char)(RAM[ACC] & 0x0F) <
			(unsigned char)((RAM[RAM[GetRegisterBank()+regNum]] & 0x0F) + 
			 (char)GetBit(RAM[PSW], CY)) ) {
			borrow3 = true;
		    }
		    if( (unsigned char)(RAM[ACC] & 0x7F) <
			(unsigned char)((RAM[RAM[GetRegisterBank()+regNum]] & 0x7F) + 
			 (char)GetBit(RAM[PSW], CY)) ) {
			borrow6 = true;
		    }
		    if( (unsigned short)(unsigned char)RAM[ACC] <
			((unsigned short)(unsigned char)RAM[RAM[GetRegisterBank()+regNum]] + 
			 (unsigned short)GetBit(RAM[PSW], CY)) ) {
			borrow7 = true;
		    }
		    RAM[ACC] = (unsigned short)(unsigned char)RAM[ACC] - 
			((unsigned short)(unsigned char)RAM[RAM[GetRegisterBank()+regNum]] + 
			 (unsigned short)GetBit(RAM[PSW], CY));
		    if( borrow3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( borrow7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (borrow6 && !borrow7) || (!borrow6 && borrow7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A - (B + C)= ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << borrow3 << " " << borrow6 << " " << borrow7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // SUBB A, (#data)
		case SUBB4:
		    
		    #ifdef DEBUG
			os << "SUBB 4" << endl;
		    #ifdef DETAIL
			os << "\t" << "A <- A - ";
			PrintHex(ROM[PC], &os);
			os << " - C" << endl;
			os << "\t" << "A = "; PrintHex(RAM[ACC], &os); os << endl;
		    #endif
		    #endif
		    borrow3 = false;
		    borrow6 = false;
		    borrow7 = false;
		    IR = ROM[PC++];
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "B = ";
			PrintHex((char)IR, &os); 
			os << endl;
			os << "\t" << "C = ";
			PrintHex(GetBit(RAM[PSW], CY), &os);
			os << endl;
                    #endif
                    #endif
		    if( (unsigned char)(RAM[ACC] & 0x0F) <
			(unsigned char)((IR & 0x0F) + (char)GetBit(RAM[PSW], CY)) ) {
			borrow3 = true;
		    }
		    if( (unsigned char)(RAM[ACC] & 0x7F) <
			(unsigned char)((IR & 0x7F) + (char)GetBit(RAM[PSW], CY)) ) {
			borrow6 = true;
		    }
		    if( (unsigned short)(unsigned char)RAM[ACC] <
			((unsigned short)IR + (unsigned short)GetBit(RAM[PSW], CY)) ) {
			borrow7 = true;
		    }
		    RAM[ACC] = (unsigned short)(unsigned char)RAM[ACC] - 
			((unsigned short)IR + (unsigned short)GetBit(RAM[PSW], CY));
		    if( borrow3 ) SetBit(RAM[PSW], AC);
		    else ClearBit(RAM[PSW], AC);
		    if( borrow7 ) SetBit(RAM[PSW], CY);
		    else ClearBit(RAM[PSW], CY);
		    if( (borrow6 && !borrow7) || (!borrow6 && borrow7) ) 
			SetBit(RAM[PSW], OV);
		    else ClearBit(RAM[PSW], OV);
		    cycleCount += 12;
		    #ifdef DEBUG
		    #ifdef DETAIL
			os << "\t" << "A - (B + C)= ";
			PrintHex(RAM[ACC], &os); 
			os << endl;
			os << borrow3 << " " << borrow6 << " " << borrow7 << endl;
                    #endif
                    #endif
		    break;
		    
		    // Return
		case RET:
		    
		    ((unsigned char*)&PC)[1] = RAM[(unsigned char)RAM[SP]];
		    RAM[SP] = (unsigned char)RAM[SP] - 1;
		    ((unsigned char*)&PC)[0] = RAM[(unsigned char)RAM[SP]];
		    RAM[SP] = (unsigned char)RAM[SP] - 1;
		    #ifdef DEBUG
			os << "RET" << endl;
		    #ifdef DETAIL
			os << "\t" << "RET " << PC << endl;
		    #endif
		    #endif
		    cycleCount += 24;
		    break;
		    
		default:
		    cerr << "ERROR in ROM. Execution stopped." << endl;
		    progEnd = true;
		    break;
	    }
	    
	    #ifdef PORTS
		if( (lastP0 != RAM[P0]) || (lastP1 != RAM[P1]) || 
		    (lastP2 != RAM[P2]) || (lastP3 != RAM[P3]) ) {
		    lastP0 = RAM[P0];
		    lastP1 = RAM[P1];
		    lastP2 = RAM[P2];
		    lastP3 = RAM[P3];
		    PrintPorts();
		} 
	    #endif
	    
	    if( ProgramCompletion() ) progEnd = true;
	}
	
	end = clock();
	double time = (double)(end - begin)/(double)(CLOCKS_PER_SEC);
	cout << setiosflags(ios::left);
	cout << endl;
	cout << setw(45) << "Instructions Executed: " 
	     << instrCount << endl;
	cout << setw(45) << "Execution Time(seconds): " 
	     << time << endl;
	cout << setw(45) << "Average Instructions/second: " 
	     << instrCount/time << endl;
	cout << endl;
	cout << setw(45) << "Clock Cycles Required for 8051: " 
	     << cycleCount << endl;
	cout << setw(45) << "Execution Time for 8051(12 MHz)(seconds): " 
	     << cycleCount/12e6 << endl;
	cout << setw(45) << "Average Instructions/second for 8051: " 
	     << instrCount/(cycleCount/12e6) << endl;
	cout << endl;
	return true;
    }
}    

//-----------------------------------------------------------------------------

bool
I8051::ProgramCompletion() 
{
    #ifdef PROGRAM_COMPLETION
    if( PROGRAM_COMPLETION ) return true;
    #endif
    return false;
}

//-----------------------------------------------------------------------------

void
I8051::Stop() 
{
    progEnd = true;
}

//-----------------------------------------------------------------------------

// Initialize special areas of memory to specified values.
// 8051 default values for Special Purpose Registers.

void
I8051::Init8051() {
    
    for(unsigned int i=0;i<RamSize;i++) RAM[i] = 0x00;
    //for(unsigned int i=0;i<XRamSize;i++) XRAM[i] = 0x00;

    // initialize SFR
    RAM[ACC] = 0x00;
    RAM[PSW] = 0x00;
    RAM[P0] = 0xFF;
    RAM[P1] = 0xFF;
    RAM[P2] = 0xFF;
    RAM[P3] = 0xFF;
    RAM[B] = 0x00;
    RAM[SP] = 0x07;
    RAM[DPH] = 0x00;
    RAM[DPL] = 0x00;

    #ifdef PORTS
	cout << "P0\tP1\tP2\tP3" << endl;
	PrintPorts();
    #endif
}

//-----------------------------------------------------------------------------

// Decodes the given instruction and returns the instruction type.

Opcode 
I8051::Decode(const unsigned char IR) 
{    
    // BBBBBBBB
    switch( IR ) {
	case 0x25:
	    return ADD2;
	case 0x24:
	    return ADD4;
	case 0x35:
	    return ADDC2;
	case 0x34:
	    return ADDC4;
	case 0x55:
	    return ANL2;
	case 0x54:
	    return ANL4;
	case 0x52:
	    return ANL5;
	case 0x53:
	    return ANL6;
	case 0x82:
	    return ANL7;
	case 0xB0:
	    return ANL8;
	case 0xB5:
	    return CJNE1;
	case 0xB4:
	    return CJNE2;
	case 0xE4:
	    return CLR1;
	case 0xC3:
	    return CLR2;
	case 0xC2:
	    return CLR3;
	case 0xF4:
	    return CPL1;
	case 0xB3:
	    return CPL2;
	case 0xB2:
	    return CPL3;
	case 0xD4:
	    return DA;
	case 0x14:
	    return DEC1;
	case 0x15:
	    return DEC3;
	case 0x84:
	    return DIV;
	case 0xD5:
	    return DJNZ2;
	case 0x04:
	    return INC1;
	case 0x05:
	    return INC3;
	case 0xA3:
	    return INC5;
	case 0x20:
	    return JB;
	case 0x10:
	    return JBC;
	case 0x40:
	    return JC;
	case 0x73:
	    return JMP;
	case 0x30:
	    return JNB;
	case 0x50:
	    return JNC;
	case 0x70:
	    return JNZ;
	case 0x60:
	    return JZ;
	case 0x12:
	    return LCALL;
	case 0x02:
	    return LJMP;
	case 0xE5:
	    return MOV2;
	case 0x74:
	    return MOV4;
	case 0xF5:
	    return MOV8;
	case 0x85:
	    return MOV10;
	case 0x75:
	    return MOV12;
	case 0xA2:
	    return MOV16;
	case 0x92:
	    return MOV17;
	case 0x90:
	    return MOV18;
	case 0x93:
	    return MOVC1;
	case 0x83:
	    return MOVC2;
	case 0xE0:
	    return MOVX2;
	case 0xF0:
	    return MOVX4;
	case 0xA4:
	    return MUL;
	case 0x45:
	    return ORL2;
	case 0x44:
	    return ORL4;
	case 0x42:
	    return ORL5;
	case 0x43:
	    return ORL6;
	case 0x72:
	    return ORL7;
	case 0xA0:
	    return ORL8;
	case 0xD0:
	    return POP;
	case 0xC0:
	    return PUSH;
	case 0x22:
	    return RET;
	case 0x32:
	    return RETI;
	case 0x23:
	    return RL;
	case 0x33:
	    return RLC;
	case 0x03:
	    return RR;
	case 0x13:
	    return RRC;
	case 0xD3:
	    return SETB1;
	case 0xD2:
	    return SETB2;
	case 0x80:
	    return SJMP;
	case 0x95:
	    return SUBB2;
	case 0x94:
	    return SUBB4;
	case 0xC4:
	    return SWAP;
	case 0xC5:
	    return XCH2;
	case 0x65:
	    return XRL2;
	case 0x64:
	    return XRL4;
	case 0x62:
	    return XRL5;
	case 0x63:
	    return XRL6;
	default:
	    break;
    }
    
    // BBBBBBBX
    switch( IR & 0xFE ) {
	case 0x26:
	    return ADD3;
	case 0x36:
	    return ADDC3;
	case 0x56:
	    return ANL3;
	case 0xB6:
	    return CJNE4;
	case 0x16:
	    return DEC4;
	case 0x06:
	    return INC4;
	case 0xE6:
	    return MOV3;
	case 0x86:
	    return MOV11;
	case 0xF6:
	    return MOV13;
	case 0xA6:
	    return MOV14;
	case 0x76:
	    return MOV15;
	case 0xE2:
	    return MOVX1;
	case 0xF2:
	    return MOVX3;
	case 0x46:
	    return ORL3;
	case 0x96:
	    return SUBB3;
	case 0xC6:
	    return XCH3;
	case 0xD6:
	    return XCHD;
	case 0x66:
	    return XRL3;
	    
	default:
	    break;
    }
    
    // BBBBBXXX
    switch( IR & 0xF8 ) {
	case 0x28:
	    return ADD1;
	case 0x38:
	    return ADDC1;
	case 0x58:
	    return ANL1;
	case 0xB8:
	    return CJNE3;
	case 0x18:
	    return DEC2;
	case 0xD8:
	    return DJNZ1;
	case 0x08:
	    return INC2;
	case 0xE8:
	    return MOV1;
	case 0xF8:
	    return MOV5;
	case 0xA8:
	    return MOV6;
	case 0x78:
	    return MOV7;
	case 0x88:
	    return MOV9;
	case 0x48:
	    return ORL1;
	case 0x98:
	    return SUBB1;
	case 0x68:
	    return XRL1;
	case 0xC8:
	    return XCH1;
	    
	default:
	    break;
    }

    // XXXBBBBB
    switch( IR & 0x1F ) {
	case 0x11:
	    return ACALL;
	case 0x01:
	    return AJMP;
	default:
	    break;
    }
    
    return NOP;
}

//-----------------------------------------------------------------------------

// Set single bit of the given byte at the given location.

void 
I8051::SetBit(char &thisByte, unsigned char thisBit) 
{    
    thisByte |= (0x01 << thisBit);
}

//-----------------------------------------------------------------------------

// Clear single bit of the given byte at the given location.

void 
I8051::ClearBit(char &thisByte, unsigned char thisBit) 
{
    thisByte &= ~(0x01 << thisBit);
}

//-----------------------------------------------------------------------------

// Return value of bit as 0 or 1.

unsigned char
I8051::GetBit(char thisByte, unsigned char thisBit) 
{
    return( (thisByte & (0x01 << thisBit)) >> thisBit);
}

//-----------------------------------------------------------------------------

// Returns the integer value for the current register bank.

unsigned char 
I8051::GetRegisterBank() 
{
    return 8 * ((RAM[PSW] & 0x18) >> 3);
}

//-----------------------------------------------------------------------------

void
I8051::PrintHex(unsigned char byte, ostream* os) 
{
    unsigned char upperNibble = (byte & 0xF0) >> 4;
    unsigned char lowerNibble = byte & 0x0F;
    
    upperNibble = (upperNibble > 9) ? 'A'+(upperNibble-10) : '0'+upperNibble;
    lowerNibble = (lowerNibble > 9) ? 'A'+(lowerNibble-10) : '0'+lowerNibble;
    *os << "0x" << upperNibble << lowerNibble;
}

//-----------------------------------------------------------------------------

void
I8051::PrintPorts() 
{
    PrintHex(RAM[P0]); cout << "\t";
    PrintHex(RAM[P1]); cout << "\t";
    PrintHex(RAM[P2]); cout << "\t";
    PrintHex(RAM[P3]); cout << "\n";
}

//-----------------------------------------------------------------------------

bool
I8051::Hex2Short(const char* buf, unsigned &val) 
{    
    int i;
    
    if( sscanf(buf, "%x", &i) != 1 ) {
        cerr << "Error: hex file error." << endl;
        return false;
    }
    val = i;
    return true;
}

//----------------------------------------------------------------------------

bool 
I8051::Load(const char* buf, unsigned char* rom, unsigned& prgSize) 
{   
    unsigned temp;
    unsigned len, base, type;
    unsigned char checksum = 0;
    char hex[16];
    
    if( buf[0] != ':' ) {
        
        cerr << "Error: hex file error." << endl;
        return false;
    }
    
    hex[0] = buf[1];
    hex[1] = buf[2];
    hex[2] = 0;
    if( !Hex2Short(hex, len) ) return false;
    
    hex[0] = buf[3];
    hex[1] = buf[4];
    hex[2] = buf[5];
    hex[3] = buf[6];
    hex[4] = 0;
    if( !Hex2Short(hex, base) ) return false;
    
    hex[0] = buf[7];
    hex[1] = buf[8];
    hex[2] = 0;
    if( !Hex2Short(hex, type) ) return false;
    
    if( type == 1 ) return true;
    
    if ( (base+len) > (prgSize) ) {
        (prgSize) = base + len + 2;
    }
    
    if( (prgSize) >= RomSize || (base+len) >= RomSize ) {
        printf("program too large\n");
        exit(1);
    }
    
    for(unsigned i=0; i<len; i++) {
        hex[0] = buf[ 9 + i * 2];
        hex[1] = buf[10 + i * 2];
	if( !Hex2Short(hex, temp) ) return false;
        rom[base + i] = (unsigned char)temp;
    }
    
    //-----------------------------------------------------------

    for(unsigned i=0; buf[i * 2 + 1] && buf[i * 2 + 2]; i++) {
        
        hex[0] = buf[i * 2 + 1];
        hex[1] = buf[i * 2 + 2];
	unsigned temp;
	if( !Hex2Short(hex, temp) ) return false;
        checksum += (unsigned char)temp;
    }
    
    if( checksum != 0 ) {
        
        cerr << "Error: checksum failed." << endl;
        return false;
    }
    return false;
}

//-----------------------------------------------------------------------------

bool
I8051::LoadHex(const char *filename) 
{

    unsigned prgSize = 0;        
    char buf[256];
    ifstream ifs(filename);

    memset(ROM, 0, sizeof(char)*RomSize);
    
    if( ifs.bad() ) {
        
	cerr << "Error: invalid HEX file." << endl;
	return false;
    }
    while( !ifs.eof() ) {
	
	ifs.getline(buf, sizeof(buf));
	if( Load(buf, (unsigned char*)ROM, prgSize) ) break;
    }
    return true;
}

//-----------------------------------------------------------------------------
