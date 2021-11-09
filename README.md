# helloworld_snes
A hello world for the Super Nintendo and a 2bpp tile editor, customized for urban Steffen 

Depends on CC65 cross assembler:
Mac: `brew install cc65`
Ubuntu: `apt-get install cc65`

Build:
```
ca65 --cpu 65816 -o steffen.o steffen.s
ld65 -C mem.map steffen.o -o steffen.smc
```

The smc can be put on an everdrive to run on a real SNES. You can open it with a SNES emulator like SNES9x as well.

This program has no other purpose as to be a special farewell message for Steffen U.

The tileeditor.html is a simple editor for 2bpp tiles. It spits out palette data and tile data for snes. 2^2 is 4, 2bpp means 4 colors. Not more not less. Just a crude prototype. Sry.
