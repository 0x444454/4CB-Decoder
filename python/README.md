This is the Python batch decoder, which works for all target computers (see below on how to prepare executable file).

Input:  A video file of an episode to decode.
        Supported video formats: Any resolution, both interlaced (25i) and progressive (50p).

Output: A binary file with the decoded stream.
        To convert to an executable file for the specific home computer, you'll need to add a header.
        E.g. To load this as a PRG file on a Commodore 64, add these two bytes at the beginning of the binary file: 0x01, 0x08.


IMPORTANT: De-interlaced versions of the original episodes will NOT work unless containing all 50 frames per second (i.e. no fields were merged).
           This is because most de-interlacing algorithms are lossy and the original videos transmit a bit for each field (50 Hz).

Default configurations are for:
- Episode: "Programme 5, Take 1" (the C64 program, starting at 4:08)
- Resolution: 720x576 interlaced
- Dot position: 579, 440 (y coordinate MUST be even for interlaced video)

Change defaults int the code to match your video file.
