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

import java.io.*;
import java.awt.*;
import javax.swing.*;
import java.awt.image.*;
import java.util.LinkedList;

public class RNACanvas extends JComponent {
    public static final byte BLACK      =  0;
    public static final byte RED        =  1;
    public static final byte GREEN      =  2;
    public static final byte YELLOW     =  3;
    public static final byte BLUE       =  4;
    public static final byte MAGENTA    =  5;
    public static final byte CYAN       =  6;
    public static final byte WHITE      =  7;
    public static final byte TRANSPARENT=  8;
    public static final byte OPAQUE     =  9;
    public static final byte EMPTYBUCKET= 10;
    public static final byte MOVE       = 11;
    public static final byte TURNL      = 12;
    public static final byte TURNR      = 13;
    public static final byte MARK       = 14;
    public static final byte LINE       = 15;
    public static final byte FILL       = 16;
    public static final byte ADDBMP     = 17;
    public static final byte COMPOSE    = 18;
    public static final byte CLIP       = 19;

    private static final boolean interactive = false;

    BufferedImage image;
    int[][] bitmaps = new int[10][600*600];
    int numbmp;
    /* colorbucket */
    int r,g,b,a;   
    int numc, numa;
    /* cached color */
    boolean colorvalid = false;
    int currentColor;
    Color currentAwtColor;
    int oldcolor;
    int curx,cury,markx,marky,dir;
    boolean guiStarted = false;

    public final static int COL_TRANSPARENT = 0x00000000;

    public RNACanvas() {
	setMinimumSize(new Dimension(600,600));
	setMaximumSize(new Dimension(600,600));
	setPreferredSize(new Dimension(600,600));
	image = new BufferedImage(600,600, BufferedImage.TYPE_INT_ARGB_PRE);
	dir = 1; curx = 0; cury = 0; markx = 0; marky = 0;
	numbmp = 0; 
	clearBitmap();
    }

    void computeColor() {
	int avgA = (numa == 0 ? 255 : a / numa);
	currentColor = avgA << 24;
	if (numc > 0) {
	    currentColor += ((r/numc*avgA/255)<<16)
		+ ((g/numc*avgA/255) << 8)
		+ (b/numc*avgA/255);
	    currentAwtColor = new Color(r/numc,g/numc,b/numc,avgA);
	} else {
	    currentAwtColor = new Color(0,0,0,avgA);
	}
	colorvalid = true;
    }

    void line() {
	int deltax = markx-curx;
	int deltay = marky-cury;
	int d = Math.max(Math.abs(deltax), Math.abs(deltay));
	int c = (deltax * deltay <= 0 ? 1 : 0);
	int x = curx * d + (d-c)/2;
	int y = cury * d + (d-c)/2;
	for (int i = 0; i < d; i++) {
	    bitmaps[numbmp][x/d+600*(y/d)] = currentColor;
	    x += deltax;
	    y += deltay;
	}
	bitmaps[numbmp][markx+600*marky] = currentColor;
    }

    static class ScanLine {
	int x0, x1, y, dy;
	ScanLine(int x0, int x1, int y, int dy) {
	    this.x0 = x0;
	    this.x1 = x1;
	    this.y  = y;
	    this.dy = dy;
	}
    }

    void quickfill(Graphics g) {
	int[] bitmap = bitmaps[numbmp];
	oldcolor = bitmap[curx+600*cury];
	if (oldcolor == currentColor)
	    return;
	LinkedList todo = new LinkedList();
	int x0, x1, y;
	x0 = curx;
	x1 = curx+1;
	y = cury;
	while (x0 >= 0 && bitmap[x0+600*y] == oldcolor) {
	    bitmap[x0+600*y] = currentColor;
	    x0--;
	}
	x0++;
	while (x1 < 600 && bitmap[x1+600*y] == oldcolor) {
	    bitmap[x1+600*y] = currentColor;
	    x1++;
	}
	x1--;
	if (g != null)
	    g.drawLine(x0, y, x1, y);

	todo.add(new ScanLine(x0, x1, y, +1));
	todo.add(new ScanLine(x0, x1, y, -1));
	while (todo.size() > 0) {
	    ScanLine line = (ScanLine) todo.remove(0);
	    y = line.y + line.dy;
	    if (y < 0 || y >= 600)
		continue;
	    x0 = line.x0;
	    x1 = line.x0+1;
	    while (x0 >= 0 && bitmap[x0+600*y] == oldcolor) {
		bitmap[x0+600*y] = currentColor;
		x0--;
	    }
	    x0++;
	    if (x0 < x1) {
		while (x1 < 600 && bitmap[x1+600*y] == oldcolor) {
		    bitmap[x1+600*y] = currentColor;
		    x1++;
		}
		x1--;
		if (g != null)
		    g.drawLine(x0, y, x1, y);
		todo.add(new ScanLine(x0, x1, y, line.dy));
		if (x0 < line.x0-1 || x1 > line.x1+1)
		    todo.add(new ScanLine(x0, x1, y, -line.dy));
		x1++;
	    }
	    
	    while (x1 <= line.x1) {
		while (x1 < line.x1 && bitmap[x1+600*y] != oldcolor)
		    x1++;
		x0 = x1;
		while (x1 < 600 && bitmap[x1+600*y] == oldcolor) {
		    bitmap[x1+600*y] = currentColor;
		    x1++;
		}
		x1--;
		if (x1 >= x0) {
		    if (g != null)
			g.drawLine(x0, y, x1, y);
		    todo.add(new ScanLine(x0, x1, y, line.dy));
		    if (x1 > line.x1+1)
			todo.add(new ScanLine(x0, x1, y, -line.dy));
		}
		x1 += 2;
	    }
	}
    }

    void clearBitmap() {
	for (int i = 0; i < 600*600; i++) {
	    bitmaps[numbmp][i] = COL_TRANSPARENT;
	}
    }

    void clipBitmap() {
	for (int p = 0; p < 600*600; p++) {
	    int alpha = bitmaps[numbmp][p] >>> 24;
	    if (alpha == 255)
		continue;
	    int newcol = 0;
	    if (alpha != 0) {
		int bmp1col = bitmaps[numbmp-1][p];
		newcol = 
		    (((bmp1col >>> 24) * alpha/255) << 24) +
		    ((((bmp1col >> 16) & 0xff) * alpha/255) << 16) +
		    ((((bmp1col >> 8) & 0xff) * alpha/255) << 8) +
		    ((bmp1col & 0xff) * alpha/255);
	    }
	    bitmaps[numbmp-1][p] = newcol;
	}
    }

    private int layer = 0;
    void composeBitmap() {
// 	if (numbmp == 1) {
// 	    try {
// 		OutputStream os = 
// 		    new BufferedOutputStream(new FileOutputStream("layer"+layer+".pam"));
// 		layer++;
// 		os.write("P7\nWIDTH 600\nHEIGHT 600\nDEPTH 4\nMAXVAL 255\nTUPLTYPE RGB_ALPHA\nENDHDR\n".getBytes());
// 		for (int p= 0; p < 600*600; p++) {
// 			int a = (bitmaps[1][p] >> 24) & 0xff;
// 			if (a == 0) {
// 			    os.write(new byte[4]);
// 			} else {
// 			    os.write((((bitmaps[1][p] >> 16) & 0xff)*255+a-1)/a);
// 			    os.write((((bitmaps[1][p] >>  8) & 0xff)*255+a-1)/a);
// 			    os.write((((bitmaps[1][p]      ) & 0xff)*255+a-1)/a);
// 			    os.write(((bitmaps[1][p] >> 24) & 0xff));
// 			}
// 		}
// 		os.close();
// 	    } catch (IOException ex) {
// 		System.err.println("Cannot write Image: "+ex);
// 	    }
// 	}
	for (int p = 0; p < 600*600; p++) {
	    int bmp2col = bitmaps[numbmp][p];
	    if ((bmp2col & 0xff000000) == 0)
		continue;
	    int alpha = 255-(bmp2col >>> 24);
	    int bmp1col = bitmaps[numbmp-1][p];
	    int newcol = bmp2col + 
		(((bmp1col >>> 24) * alpha/255) << 24) +
		((((bmp1col >> 16) & 0xff) * alpha/255) << 16) +
		((((bmp1col >> 8) & 0xff) * alpha/255) << 8) +
		    ((bmp1col & 0xff) * alpha/255);
	    bitmaps[numbmp-1][p] = newcol;
	}
    }

    public synchronized void paintComponent(Graphics g) {
	g.setColor(Color.BLACK);
	g.fillRect(0,0,600,600);
	for (int i = 0; i <= (interactive ? numbmp : 0); i++) {
	    image.setRGB(0, 0, 600, 600, bitmaps[i], 0, 600);
	    g.drawImage(image, 0, 0, null, null);
	}
    }


    public synchronized void addCommand(byte cmd) {
// 	if (numCommands == commands.length) {
// 	    byte[] newcommands = new byte[commands.length*2];
// 	    System.arraycopy(commands, 0, newcommands,0, numCommands);
// 	    commands = newcommands;
// 	}
	Graphics graphics = null;
	if (cmd < TRANSPARENT) {
	    numc++;
	    if ((cmd & RED) != 0)
		r += 255;
	    if ((cmd & GREEN) != 0)
		g += 255;
	    if ((cmd & BLUE) != 0)
		b += 255;
	    colorvalid = false;
	} else if (cmd < EMPTYBUCKET) {
	    numa++;
	    if (cmd == OPAQUE)
		a+=255;
	    colorvalid = false;
	} else if (cmd == EMPTYBUCKET) {
	    numa = numc = r = g = b = a = 0;
	    colorvalid = false;
	} else if (cmd == MOVE) {
	    switch (dir) {
		case 0: cury = (cury+599)%600; break;
		case 1: curx = (curx+  1)%600; break;
		case 2: cury = (cury+  1)%600; break;
		case 3: curx = (curx+599)%600; break;
	    }
	} else if (cmd == TURNR) {
	    dir = (dir+1)&3;
	} else if (cmd == TURNL) {
	    dir = (dir-1)&3;
	} else if (cmd == MARK) {
	    markx = curx;
	    marky = cury;
	} else if (cmd == LINE) {
	    if (!colorvalid)
		computeColor();
	    line();
	    if (interactive || numbmp == 0) {
		graphics = getGraphics();
		if (graphics != null) {
		    graphics.setColor(currentAwtColor);
		    graphics.drawLine(markx, marky, curx, cury);
		}
	    }
	} else if (cmd == FILL) {
	    if (!colorvalid)
		computeColor();
	    if (interactive || numbmp == 0) {
		graphics = getGraphics();
		if (graphics != null) {
		    graphics.setColor(currentAwtColor);
		}
	    }
	    quickfill(graphics);
	} else if (cmd == ADDBMP) {
	    if (numbmp < 9) {
		numbmp++;
		clearBitmap();
	    }
	} else if (cmd == COMPOSE) {
	    if (numbmp > 0) {
		composeBitmap();
		numbmp--;
		if (guiStarted && (interactive || numbmp == 0)) {
		    repaint();
		}
	    }
	} else if (cmd == CLIP) {
	    if (numbmp > 0) {
		clipBitmap();
		numbmp--;
		if (guiStarted && (interactive || numbmp == 0)) {
		    repaint();
		}
	    }
	}
    }


    public void dumpImage() {
	try {
	    OutputStream os = 
		new BufferedOutputStream(new FileOutputStream("out.ppm"));
	    os.write("P6 600 600 255\n".getBytes());
	    for (int p = 0; p < 600*600; p++) {
		int rgba = bitmaps[0][p];
		os.write(((rgba >> 16) & 0xff));
		os.write(((rgba >>  8) & 0xff));
		os.write(((rgba      ) & 0xff));
	    }
	    os.close();
	} catch (IOException ex) {
	    System.err.println("Cannot write Image: "+ex);
	}
    }

}

