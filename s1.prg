/*
  S1 Test Program
*/

          bgn   "s1"                  // Begin of Program "s1"
          bpm   178.0                 // Set sequencer BPM 
          sig   8,8                   // Set sequencer TimeSignature
          sel   a1                    // select a1 as current context/setup/..?   TODO: find suiting name 
          ld    "audio.wav"           // Load audio.wav into current workspace
          fit   1.0
          sig   8,8                  // Set sequencer TimeSignature


          mrk   mst,0.0
          srst 
start:    
          playf ms
          wait 0:4:0
          seek 0.5
          wait 0:2:0
          jmp start
h:                                    // label
          hlt                         // halt program execution

