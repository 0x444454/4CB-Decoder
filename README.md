# 4CB-Decoder
Decoders for the mysterious computer programs encoded using a flashing dot during broadcasted TV episodes of "4 Computer Buffs" on Channel 4.

The year was 1985, and some of these episodes transmitted executable programs to be received by means of a simple DIY device with a light sensor connected to a home computer.
Supported computers were C64, BBC Micro, and ZX Spectrum.
The PAL video signal contained a white flashing dot in the lower right corner of the screen. The dot transmitted one bit per PAL field (50 bits per second).  
As far as we know, for different reasons, no one ever succeded decoding the mysterious programs using their home computers.

This project provides functional realtime and batch decoders that work with the original broadcasted signals and simulated surrogates.
So we can finally unravel the mystery ;-)

Currently supported:
- Python batch decoder: Decode a video file in the comfort of your modern machine.
- C64 realtime decoder: Decode the video in realtime using your Commodore 64 and the original hardware and light sensor as described in the TV show. **Not for the faint of heart.**

TODO: Support for BBC Micro and ZX Spectrum. As of now, nobody has ever decoded their mystery programs.

Read the story of the reverse-engineering of the transmission protocol in this thread on Lemon64 (spoiler warning):
https://www.lemon64.com/forum/viewtopic.php?t=85204

Watch the videos by Perifractic about this decoding adventure:  
https://www.youtube.com/watch?v=MezkfYTN6EQ  
https://www.youtube.com/watch?v=VRcs_TUpQ6g  (spoiler warning)  

