/* Copyright 2007 by Jochen Hoenicke
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id$
 */

import java.awt.*;
import javax.swing.*;

public class Visualizer {
    public final static byte BLACK      =  0;
    public final static byte RED        =  1;
    public final static byte GREEN      =  2;
    public final static byte YELLOW     =  3;
    public final static byte BLUE       =  4;
    public final static byte MAGENTA    =  5;
    public final static byte CYAN       =  6;
    public final static byte WHITE      =  7;
    public final static byte TRANSPARENT=  8;
    public final static byte OPAQUE     =  9;
    public final static byte EMPTYBUCKET= 10;
    public final static byte MOVE       = 11;
    public final static byte TURNL      = 12;
    public final static byte TURNR      = 13;
    public final static byte MARK       = 14;
    public final static byte LINE       = 15;
    public final static byte FILL       = 16;
    public final static byte ADDBMP     = 17;
    public final static byte COMPOSE    = 18;
    public final static byte CLIP       = 19;

    private RNACanvas canvas;

    public Visualizer() {
	canvas = new RNACanvas();
    }

    public void startGui() {
	JFrame frame = new JFrame("Endomorphose");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	frame.getContentPane().add(canvas);
	frame.pack();
	frame.setVisible(true);
	canvas.guiStarted = true;
    }

    public void dumpImage() {
	canvas.dumpImage();
    }

    /*@ requires rna != null && rna.length == 7;
      @*/
    public void addRNA(int cmd) {
	if (Main.verboseLevel > 0) 
	    System.err.println("RNA: "+Integer.toString(cmd, 4));
	switch (cmd) {
	    case 0x3301: /*PIPIIIC*/
		canvas.addCommand(BLACK);
		break;
	    case 0x3303: /*PIPIIIP*/
		canvas.addCommand(RED);
		break;
	    case 0x3305: /*PIPIICC*/
		canvas.addCommand(GREEN);
		break;
	    case 0x3306: /*PIPIICF*/
		canvas.addCommand(YELLOW);
		break;
	    case 0x3307: /*PIPIICP*/
		canvas.addCommand(BLUE);
		break;
	    case 0x3309: /*PIPIIFC*/
		canvas.addCommand(MAGENTA);
		break;
	    case 0x330a: /*PIPIIFF*/
		canvas.addCommand(CYAN);
		break;
	    case 0x330d: /*PIPIIPC*/
		canvas.addCommand(WHITE);
		break;
	    case 0x330e: /*PIPIIPF*/
		canvas.addCommand(TRANSPARENT);
		break;
	    case 0x330f: /*PIPIIPP*/
		canvas.addCommand(OPAQUE);
		break;
	    case 0x30c7: /*PIIPICP*/
		canvas.addCommand(EMPTYBUCKET);
		break;
	    case 0x3003: /*PIIIIIP*/
		canvas.addCommand(MOVE);
		break;
	    case 0x3557: /*PCCCCCP*/
		canvas.addCommand(TURNL);
		break;
	    case 0x3aab: /*PFFFFFP*/
		canvas.addCommand(TURNR);
		break;
	    case 0x352b: /*PCCIFFP*/
		canvas.addCommand(MARK);
		break;
	    case 0x3a17: /*PFFICCP*/
		canvas.addCommand(LINE);
		break;
	    case 0x30c3: /*PIIPIIP*/
		canvas.addCommand(FILL);
		break;
	    case 0x35eb: /*PCCPFFP*/
		canvas.addCommand(ADDBMP);
		break;
	    case 0x3ad7: /*PFFPCCP*/
		canvas.addCommand(COMPOSE);
		break;
	    case 0x3a16: /*PFFICCF*/
		canvas.addCommand(CLIP);
		break;
	}
    }

}
