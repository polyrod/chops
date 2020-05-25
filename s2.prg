/*
  S1 Test Program
*/

          bgn   "s1"                  // Begin of Program "s1"
          bpm   240                   // Set sequencer BPM 
          bpm   240.5                 // Set sequencer BPM 
          sig   4,4                   // Set sequencer TimeSignature
          sel   a1                    // select a1 as current context/setup/..?   TODO: find suiting name 
          ld    "audio.wav"           // Load audio.wav into current workspace
start:                                // label
          set   x , 13  set r,6.4     // set two variables in one line
          jmp   middle                // continue at label "middle"
ge:                                   // label    
          //sel   a2                    // select workspace a2
          waitt 04:0:000               // wait for 4 beats ; beat is defined by sequencer/timesignature
          set   cx,5                  // set up a loop counter variable
          set   cx,5.1                // set up a loop counter variable
          jmp   loop                  // enter the loop @ label "loop"

middle:   //sel   a3                    // select different workspace
          wait  01:00:000             // wait for 1 bar
          set   x , 15.1              // (re)set value for var "x"
          set   y , (x + 1) * 6       // set y to result of expression
          set   z , y / 32.0          // set z to result of expression
          set   w, 3 + -4 * 8         // some expressions
          set   o, 5.0 * 2.4          // some expressions
          jmp   ge                    // continue at "ge"
en:                                   // label "en"
          //sel   a4                    // select a workspace
          wait  0:00:384              // wait for 4 beats @96 pulses ber beat
          jmp   h                     // jump
loop:                                 // tha loop
          wait  0:001:0               // wait a beat
          set   u, o / cx             // some expressions
          set   v, 4.3 * cx           // some expressions 
          set   g,  5 / 4             // some expressions
          sel   a4                    // select a workspace
          jdnz cx , loop              // decrement counter , if counter not zero reloop
          set   g,  5 / 4             // some expressions
          jmp start
          
h:                                    // label
          hlt                         // halt program execution

