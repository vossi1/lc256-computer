# LC256 - CBM style 256 color SMD computer

**Copyright (c) 2025 Vossi - v 1.2**
**www.mos6509.com**

## License
This work is licensed under a Creative Commons Attribution-ShareAlike 4.0
International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).

![LC256 start screen](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_title.jpg)

Lena - YJK-mode with 19k colors, 63c09 starwars-test-ROM:  

![LC256 Lena](https://github.com/vossi1/lc256-computer/blob/master/pictures/yjk_lena.jpg) ![LC256 6309](https://github.com/vossi1/lc256-computer/blob/master/pictures/starwars6309.jpg)

**description:**

    A 4MHz 65c02 computer with the Yamaha V9958 (V9938)
    - 1MB RAM (256/512KB possible)
    - 32KB ROM with kernal/Basic4+ (incl. graphics-statements)
    - 128kB dedicated video RAM
    - IEC-interface
    - onboard IEC2SD-drive
    - RGB, YC, composite output in PAL/NTSC (V9938 only RGB!)
    - 2 internal extension-connectors
    - external cartridge connector for development cart
    - optional DAC/OPL3-soundcard
    - optional HD63c09 CPU-adapter - needs special CPLD-code and DOS-ROM (under development)
    - fits in c64/c64c-case with c64-keyboard

**v1.2 changes:**

    - Dual red/green case LED (orange = power, green = SD read, red = SD record/error)
    - optional 64K ROM with 32K bank switching with VIA2 PA2
    - WDC 65c22s pullup resistors (VIA1)
    - V9938 RGB capacitor bypass (JP24-26)
    - minor layout corrections (composite and DIN connector holes)

**[Schematic](https://github.com/vossi1/lc256-computer/blob/master/schematics/lc256_v12.png)**

**[Parts](https://github.com/vossi1/lc256-computer/blob/master/parts/lc256_v12_parts.txt)**

The pcb (initial release v1.1) fits exactly in a C64C-case:
![LC256 photo](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_case.jpg)

**details:**

    The LC256 runs with 4MHz PLCC CMD/Rockwell 65c02 and CMD/Rockwell 65c22 or WDC 65c22.
    For a WDC65C02S CPU you have to change the solderpards JW1-4 and assemble the R62.
    To use WDC65c22s with v1.1 you need the two resistor-packs-patch on the back side (photo)
    You should use a EPM7064SLC44 CPLD (7032 possible with adapted code). With limited
    DMA-features it's possible to use a GAL 26CV12 instead (solderpad change needed!).
    The NE555 circuit is not needed if you use the VIA-restore (like in the VIC20).
    For 256/512KB-chips you need IC23. With 128KB chips you can use D1-4/R25,26 instead.
    The 8MHz oscillator is also used for the SD2IEC. You can select 1/2/4 MHz CPU clock.
    With 1 or 2MHz you need an especially assembled ROM for the IEC-timings.
    The OSC2 is optional for the 63c09 or other custom clock frequencies...
    You have to select PAL or NTSC for composite/YC with the jumpers JP16-18.
    The 8MHz oscillator must a 3.3V type for the SD2IEC!
    The BASIC is C64/PET/CBM2-compatible except peeks/pokes!
    There are two screen modes selectable:
    - 256x212 pixel, 40 columns, 256 colors (fixed RGB)
    - 512x212 pixel, 80 columns, 16 colors (palette)

Final pcb v1.2 with HD63c09-CPU-adapter and Soundcard: 
![LC256 pcb](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_v12_6309_case.jpg)

**jumpers for 65c02, CPLD, V9958, 1MB RAM, no 555, 3.3V osc's:**

    JW1-4 (WDC-CPU): 1-2
    JP1-2 (V9958): close (PCB v1.1 preconnected, v1.2 open)
    JP3 (IEC Reset/Switch1): 1-2
    JP4 (AVR-Reset): open
    JP5 (OSC Q3 3.3/5V): 1-2 for 3.3V
    JP6 (OSC2 3.3/5V): 1-2 for 3.3V
    JP7 (VIA2 NMI/IRQ): 1-2
    JP8 (DMA/RDY): 1-2
    JP9 (555-NMI): open
    JP10 (CS2/BS-TSC): 1-2
    JP11 (CS1/BA-AVMA): 1-2
    JP12 (BA):
    JP13 (Restore CA1/555): 1-2
    JP14 (CPU-Clock): 1/2 (4MHz)
    JP15 (OSC 1/2): 1-2 (OSC1)
    JP16-18 (PAL/NTSC): 1-2 = PAL, 2-3 = NTSC
    JP19 (FIRQ(6309) Ext.1): open
    JP20 (FIRQ(6309) Ext.2): open
    JP21 (512KB RAM): 2-3
    JP22 (512KB RAM): close
    JP23 (64K ROM): open
    JP24-26 (V9938): open

Final pcb v1.2 bottom side:    
![LC256 pcb back](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_v12_pcb_back.jpg)

**assembling hints:**

    The holes for the 3,5mm composite/audio-connector are too small - you have to cut the
    pins a bit to fit. The ground-pins of the DIN-connectors doesn't fit perfectly.
    I soldered all the PLCC and SOJ sockets with solder paste and hot air.
    After soldering I checked if all pins are really fixed and no bridge exists!
    If you want to use a V9938 you have to solder a 10k resistor and open the
    V9958 solder-bridges (check the correct V9958-bridges carefully!!!).
    You can leave the 10k resistor for the V9958.
    With V9938 you have to bridge the 100nF caps C8,C9,C12. YC and composite doesn't work with V9938!
    You have to solder the YC-board on the pinheaders to get the correct height for the case!

OPL3-DAC-soundcard:
![OPL3-DAC photo](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256-opl3-dac_soundcard_v10.jpg)

Looks nice in a clear C64C-case (pcb-LEDs visible):
![LC256 case2](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_case2.jpg)

First prototype:
![LC256 prototype](https://github.com/vossi1/lc256-computer/blob/master/pictures/lc256_first-prototype-v1.0.png)
