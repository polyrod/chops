# chops ![Haskell CI](https://github.com/polyrod/chops/workflows/Haskell%20CI/badge.svg) 
C(ontrolled)H(andling)O(f)P(laying)S(amples) --- an asm style language for sample playback manipulation


## Motivation

CHOPS is the attempt to make audio construction controllable. Its not a coincident that programming computer is still
mainly a text based process instead of a visual GUI process. Modern DAWs (DigitalAudioWorkstations) have mostly superseded
oldschool trackers and offer a lot more features , still the keyboard compositing approach of trackers was a very effective
way to communicate musical ideas to the computer. Any work you do with your mouse yield a result and that result only, you loose all the steps getting there. 
Instead writing a recepie that when followed will yield a result and that can be altered at any step of the construction process because its only text.


CHOPS tries to give you the best of all these worlds, the ability to compose with your keyboard, having a textfile describing your composition that can be altered afterwards at any place
so you can even use revision control systems on your compositions as with source code, and the ability to have multiple output ports where you can hook up
audio processing plugins that you can control with your mouse andor midi controller.


## Overview

CHOPS is an asm-style language and a virtual machine that executes the CHOPS language.
Part of the VM is a Audio engine that handles:
        
        - musical time
        - samples loading 
        - samples fitting
        - playing and routing
        - audio output ports






