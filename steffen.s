.p816 ; 65816 modus
.i16  ; Indexregister 16 Bit
.a8   ; Akkumulator 8 Bit

; Variablen im Work-RAM ablegen (WRAM1 $7eXXXX), siehe mem.map
.segment "DATA"
index:          .res 1
rgb:            .res 1
tmp:            .res 1
bgr:            .res 2
count:          .res 1
in_nmi:         .res 1
x_pos:          .res 1
sin_hdma:       .res 513

; ab hier ist alles im ROM, also das was auf dem Super Nintendo Modul
; ist, was wir früher in die Konsole gesteckt haben
.segment "CODE"
SIN_TABLE: ; für das WobbleWobble :)
.byte  $80, $83, $86, $89, $8c, $90, $93, $96
.byte  $99, $9c, $9f, $a2, $a5, $a8, $ab, $ae
.byte  $b1, $b3, $b6, $b9, $bc, $bf, $c1, $c4
.byte  $c7, $c9, $cc, $ce, $d1, $d3, $d5, $d8
.byte  $da, $dc, $de, $e0, $e2, $e4, $e6, $e8
.byte  $ea, $eb, $ed, $ef, $f0, $f1, $f3, $f4
.byte  $f5, $f6, $f8, $f9, $fa, $fa, $fb, $fc
.byte  $fd, $fd, $fe, $fe, $fe, $ff, $ff, $ff
.byte  $ff, $ff, $ff, $ff, $fe, $fe, $fe, $fd
.byte  $fd, $fc, $fb, $fa, $fa, $f9, $f8, $f6
.byte  $f5, $f4, $f3, $f1, $f0, $ef, $ed, $eb
.byte  $ea, $e8, $e6, $e4, $e2, $e0, $de, $dc
.byte  $da, $d8, $d5, $d3, $d1, $ce, $cc, $c9
.byte  $c7, $c4, $c1, $bf, $bc, $b9, $b6, $b3
.byte  $b1, $ae, $ab, $a8, $a5, $a2, $9f, $9c
.byte  $99, $96, $93, $90, $8c, $89, $86, $83
.byte  $80, $7d, $7a, $77, $74, $70, $6d, $6a
.byte  $67, $64, $61, $5e, $5b, $58, $55, $52
.byte  $4f, $4d, $4a, $47, $44, $41, $3f, $3c
.byte  $39, $37, $34, $32, $2f, $2d, $2b, $28
.byte  $26, $24, $22, $20, $1e, $1c, $1a, $18
.byte  $16, $15, $13, $11, $10, $0f, $0d, $0c
.byte  $0b, $0a, $08, $07, $06, $06, $05, $04
.byte  $03, $03, $02, $02, $02, $01, $01, $01
.byte  $01, $01, $01, $01, $02, $02, $02, $03
.byte  $03, $04, $05, $06, $06, $07, $08, $0a
.byte  $0b, $0c, $0d, $0f, $10, $11, $13, $15
.byte  $16, $18, $1a, $1c, $1e, $20, $22, $24
.byte  $26, $28, $2b, $2d, $2f, $32, $34, $37
.byte  $39, $3c, $3f, $41, $44, $47, $4a, $4d
.byte  $4f, $52, $55, $58, $5b, $5e, $61, $64
.byte  $67, $6a, $6d, $70, $74, $77, $7a, $7d

; TILE_DATA ist unser Zeichensatz
TILE_DATA: ; Nur die Zeichen, die wir für unsere nette Botschaft brauchen
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; nüscht
.byte $42,$00,$42,$00,$42,$00,$7e,$00,$42,$00,$42,$00,$42,$00,$00,$00 ; H 1
.byte $7e,$00,$40,$00,$40,$00,$7c,$00,$40,$00,$40,$00,$7e,$00,$00,$00 ; E 2
.byte $40,$00,$40,$00,$40,$00,$40,$00,$40,$00,$40,$00,$7e,$00,$00,$00 ; L 3
.byte $3c,$00,$42,$00,$42,$00,$42,$00,$42,$00,$42,$00,$3c,$00,$00,$00 ; O 4
.byte $42,$00,$42,$00,$42,$00,$42,$00,$5a,$00,$5a,$00,$24,$00,$00,$00 ; W 5
.byte $7c,$00,$42,$00,$42,$00,$7c,$00,$42,$00,$42,$00,$42,$00,$00,$00 ; R 6
.byte $40,$00,$40,$00,$40,$00,$40,$00,$40,$00,$40,$00,$7e,$00,$00,$00 ; L 7
.byte $7c,$00,$42,$00,$42,$00,$42,$00,$42,$00,$42,$00,$7c,$00,$00,$00 ; D 8
.byte $42,$00,$66,$00,$5a,$00,$42,$00,$42,$00,$42,$00,$42,$00,$00,$00 ; M 9
.byte $18,$00,$24,$00,$42,$00,$7e,$00,$42,$00,$42,$00,$42,$00,$00,$00 ; A a
.byte $3c,$00,$42,$00,$40,$00,$40,$00,$40,$00,$42,$00,$3c,$00,$00,$00 ; C b
.byte $3c,$00,$42,$00,$40,$00,$3c,$00,$02,$00,$42,$00,$3c,$00,$00,$00 ; S c
.byte $3c,$00,$42,$00,$40,$00,$4e,$00,$42,$00,$42,$00,$3c,$00,$00,$00 ; G d
.byte $42,$00,$42,$00,$42,$00,$42,$00,$42,$00,$42,$00,$3c,$00,$00,$00 ; U e
.byte $7e,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$00,$00 ; T f
.byte $7e,$00,$40,$00,$40,$00,$7c,$00,$40,$00,$40,$00,$40,$00,$00,$00 ; F 10
.byte $42,$00,$62,$00,$52,$00,$4a,$00,$46,$00,$42,$00,$42,$00,$00,$00 ; N 11
.byte $08,$00,$08,$00,$10,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; ' 12
.byte $08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$00,$00 ; I 13
.byte $7c,$00,$42,$00,$42,$00,$7c,$00,$42,$00,$42,$00,$7c,$00,$00,$00 ; B 14
.byte $42,$00,$44,$00,$48,$00,$70,$00,$48,$00,$44,$00,$42,$00,$00,$00 ; K 15
.byte $00,$44,$40,$ae,$20,$de,$20,$5c,$10,$28,$00,$38,$00,$10,$00,$00 ; <3 16

TILE_MAP_1:
;       H   E   L   L   O       W   O   R   L   D
.byte $01,$02,$03,$03,$04,$00,$05,$04,$06,$07,$08
TILE_MAP_2:
;       M   A   C   H   '   S       G   U   T       S   T   E   F   F   E   N
.byte $09,$0a,$0b,$01,$12,$0c,$00,$0d,$0e,$0f,$00,$0c,$0f,$02,$10,$10,$02,$11
TILE_MAP_3:
;       D   I   E      B    N   O   T   K     <3        D   I   C   H
.byte $08,$13,$02,$00,$14,$11,$04,$0f,$15,$00,$16,$00,$08,$13,$0b,$01

; die Farbpalette, schwarz,weiß und rot für das Herz
PALETTE_DATA:
.byte $00,$00,$ff,$7f,$1f,$04

.proc   ResetHandler            ; hier springt das SNES nach dem Reset hin
	sei			; IRQs abschalten
	clc
	xce			; 16 bit aktivieren, yeah!!
        rep #$10
        sep #$20
        lda #$8f                ; Bildschirm abschalten
        sta $2100

        ; unsere Farbpalette hochladen
        ldx #$00
        stz $2121
cgram_cp:
        lda PALETTE_DATA,X
        sta $2122
        inx
        lda PALETTE_DATA,X
        sta $2122
        inx
        cpx #$06   ; 3 Farben a 2 Byte (15 bit + 1 wasted)
        bne cgram_cp

        ; unseren Zeichensatz hochladen
        lda #$80
        sta $2115       ; automatisch hochzählen bei Zugriff
        ldx #$00
        stz $2116
        stz $2117
upload_chr:
        lda TILE_DATA,X
        sta $2118
        inx
        lda TILE_DATA,X
        sta $2119
        inx
        cpx #(23*16)    ; ich habe 22 Zeichen, plus Herz am Start
        bne upload_chr

        ; Tilemap oder das was auf dem Bildschirm zu sehen ist hochladen
        ldx #$00
        lda #$10
        sta x_pos
upload_tilemap1:   ; HELLO WORLD
        lda x_pos
        sta $2116 ; VRAM Adresse
        inc
        sta x_pos
        lda #$05
        sta $2117
        lda TILE_MAP_1,X ; 0 -> VRAM ($0400)
        sta $2118
        lda #$00
        sta $2119
        inx
        cpx #11      ; 11 Zeichen :)
        bne upload_tilemap1

        ldx #$00
        lda #$09
        sta x_pos
upload_tilemap2: ; MACH'S GUT STEFFEN
        lda x_pos
        sta $2116 ; VRAM Adresse
        inc
        sta x_pos
        lda #$06
        sta $2117
        lda TILE_MAP_2,X ; 0 -> VRAM ($0400)
        sta $2118
        lda #$00
        sta $2119
        inx
        cpx #18   ; 18 Zeichen :)
        bne upload_tilemap2
         
        ldx #$00
        lda #$09
        sta x_pos
 upload_tilemap3: ; DIE BNOTK <3 DICH
        lda x_pos
        sta $2116 ; VRAM Adresse
        inc
        sta x_pos
        lda #$07
        sta $2117
        lda TILE_MAP_3,X ; 0 -> VRAM ($0400)
        sta $2118
        lda #$00
        sta $2119
        inx
        cpx #17   ; 17 Zeichen
        bne upload_tilemap3
     
        stz $2105        ; Mode 0 als Videomodus (4 Hintergründe a 4 Farben pro Tile / 2bpp)
        lda #$04         ; Adresse der Tilemap ($04xx)
        sta $2107
        stz $210B       ; Adresse des Zeichensatzes ($00xx)

        lda #$01        ; Hintergrund 1 anschalten
        sta $212c

        lda #$81
        sta $4200               ; V-Blank Interrupt an
        lda #$0f                ; Bildschirm wieder anschalten
        sta $2100
        cli                     ; Interrupts wieder an
.endproc

.proc   GameLoop                ; Loop Loop
        lda #$00
        sta count
wait_nmi:                       ; einfach warten, bis der Videochip
        lda in_nmi              ; fertig mit dem aktuellen Frame ist (V_BLANK)
check_again:                    ; wenn die NMI Routine in_nmi ändert
        wai                     ; wissen wir, dass der Videochip für eine gewisse
        cmp in_nmi              ; Zeit nichts rendert und wir können
        beq check_again         ; den nächsten Frame ohne Flackern vorbereiten

        lda count               ; in V_BLANK die Sinustabelle vorbereiten
        tax                     ; x rotiert die Sinustabelle über den Startwert
        ldy #$00
copy_sin:
        lda #$01                ; Wert gilt für eine Zeile
        sta sin_hdma,Y
        iny                     ; y+1
        lda SIN_TABLE,X
        lsr                     ; /2
        lsr                     ; /2
        sta sin_hdma,Y          ; Wert schreiben
        iny                     ; y+1
        inx                     ; x+1
        txa                     ; x->a, da X 16 bit hat
        ldx #0                  ; A hat 8 bit, x löschen
        tax                     ; a->x, der Zähler geht also von 0-255 und fängt dann
        cpy #480                ; von vorn an. 480 Bildschirmzeilen versorgt?
        bne copy_sin
        lda #$00                ; end of the hdma table
        sta sin_hdma,Y
        inc count               ; beim nächsten Frame starten wir in der Sinustabelle einen Schritt weiter
                                ; Ergebnis -> wobble

        ; HDMA macht mit der Sinustabelle das hübsche Wobbelwobbel
        ; indem die Werte Zeile für Zeile ins horizontale Scrollregister
        ; geschrieben werden, kurz bevor die Zeile gerendert wird (horizontal DMA)
        lda #$00000000          ; direct mode, direkt aus dem RAM
        sta $4300
        lda #$0d                ; horizontales Scrollregister
        sta $4301
        ldx #.loword(sin_hdma)  ; 16 bit Adresse im Segment 7e
        stx $4302
        lda #$7e                ; das ist das hibyte (Segment) der Adresse von sin_hdma
        sta $4304               ; $7eXXXX ist das RAM des Super Nintendo (WRAM1)
        lda #%00000001          ; HDMA Channel 1 aktivieren
        sta $420c               ; und go!
next:
        jmp wait_nmi            ; und nächster Frame
.endproc

.proc   NMIHandler              ; NMIHandler, called every frame/V-blank
        inc in_nmi              ; unsere Hauptroutine SteffenLoop, weiß jetzt, dass der Videochip
                                ; für eine gewisse Zeit nichts tut
        lda $4210               ; read NMI status
        rti    
.endproc


.segment "SNESHEADER"
;$00FFC0-$00FFFF

; SNES Header
.byte "Fuer Urban Steffen   " ; rom name 21 chars
.byte $30  ; LoROM FastROM
.byte $00  ; extra chips in cartridge, 00: no extra RAM; 02: RAM with battery
.byte $05  ; ROM size (2^# in kB)
.byte $00  ; backup RAM size
.byte $01  ; US
.byte $33  ; publisher id
.byte $00  ; ROM revision number
.word $0000  ; checksum of all bytes
.word $0000  ; $FFFF minus checksum

;ffe0 not used
.word $0000
.word $0000

; 65816 pointer 
;ffe4 - native mode vectors
.addr $0000  ;cop native **
.addr $0000  ;brk native **
.addr $0000  ;abort native not used *
.addr NMIHandler ;nmi native
.addr ResetHandler ;RESET native
.addr $0000 ;irq native

;fff0 not used
.word $0000
.word $0000

;fff4 - emulation mode vectors
.addr $0000  ;cop emulation **
.addr $0000 ; not used
.addr $0000  ;abort not used *
.addr NMIHandler ;nmi emulation
.addr ResetHandler ;RESET emulation
.addr $0000 ;irq/brk emulation **