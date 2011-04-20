/**
@fileOverview
Shell output functions.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved

@author Bruno Dufour (dufour@iro.umontreal.ca)
*/

var shell = shell || {};

shell.bg = shell.bg || {};
shell.fg = shell.fg || {};

shell.BLACK   = 0;
shell.RED     = 1;
shell.GREEN   = 2;
shell.YELLOW  = 3;
shell.BLUE    = 4;
shell.MAGENTA = 5;
shell.CYAN    = 6;
shell.WHITE   = 7;
shell.BOLD    = 1;

shell.NONE    = 0;

shell.fg.BLACK   = 1 + shell.BLACK;
shell.fg.RED     = 1 + shell.RED;
shell.fg.GREEN   = 1 + shell.GREEN;
shell.fg.YELLOW  = 1 + shell.YELLOW;
shell.fg.BLUE    = 1 + shell.BLUE;
shell.fg.MAGENTA = 1 + shell.MAGENTA;
shell.fg.CYAN    = 1 + shell.CYAN;
shell.fg.WHITE   = 1 + shell.WHITE;

shell.bg.BLACK   = 10 * (1 + shell.BLACK);
shell.bg.RED     = 10 * (1 + shell.RED);
shell.bg.GREEN   = 10 * (1 + shell.GREEN);
shell.bg.YELLOW  = 10 * (1 + shell.YELLOW);
shell.bg.BLUE    = 10 * (1 + shell.BLUE);
shell.bg.MAGENTA = 10 * (1 + shell.MAGENTA);
shell.bg.CYAN    = 10 * (1 + shell.CYAN);
shell.bg.WHITE   = 10 * (1 + shell.WHITE);

shell.fg.BOLD    = 100 * shell.BOLD;

shell.colorize = function(s, colors) {
    if (colors == 0) return s;

    s = s.toString();

    var fg = colors % 10;
    var bg = Math.floor(colors / 10) % 10;
    var bold = Math.floor(colors / 100) % 10;

    var cs = "";
    if (fg > 0) {
        cs = cs + "\033[";
        if (bold) cs = cs + "1;";
        cs = cs + "3" + (fg - 1) + "m";
    } else if (bold) {
        cs = cs + "\033[1m";
    }
    if (bg > 0) {
        cs = cs + "\033[4" + (bg - 1) + "m";
    }

    return cs + s + "\033[0m";
}
