# 4CB-Decoder
Decoders for the mysterious computer programs encoded using a flashing dot during broadcasted TV episodes of "4 Computer Buffs" on Channel 4.

The year was 1985 and some of these episodes transmitted executable programs to be received by means of a simple DIY device with a light sensor connected to a home computer.
Supported computers were C64, BBC Micro, and ZX Spectrum.
The PAL video signal contained a white flashing dot in the lower right corner of the screen. The dot transmitted one bit per PAL field (50 bits per second).
As far as we know, for different reasons, no one ever succeded decoding the mysterious programs using their home computers.

This project provides functional realtime and batch decoders that work with the original broadcasted signals and simulated surrogates.
So we can finally unravel the mystery ;-)

Read the story of the reverse-engineering of the transmission protocol in this thread on Lemon64 (spoiler warning):
https://www.lemon64.com/forum/viewtopic.php?t=85204
