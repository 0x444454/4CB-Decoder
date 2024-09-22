# Channel 4 broadcast dot receiver.
# This version decodes a progressive (50fps) or interlaced (25fps) source, one bit per frame.
# NOTE: - Remember to configure the code for progressive or interlaced using the "interlaced" flag.
#       - Remember to set the x and y coordinates below to the center of "the dot" in the frame.
# Version 0.8: By DDT, on 2024-08-14
# Version 0.9: By DDT, on 2024-08-15
# Version 1.0: By DDT, on 2024-08-15 (a bit later ;-)

import cv2
import os

# Global class with configurations.
class Cfg:
  # Stream type configuration
  interlaced = True               # IMPORTANT: Set to True for interlaced stream (25 fps), False for progressive (50 fps).
  start_at_ts = (60 * 4) + 8      # IMPORTANT: Timestamp to start decoding (in seconds since start of stream). This should match the "PRESS YOUR SPACE BAR NOW!" timestamp.
  stop_bit_used = True            # Set to True if the stream uses also stop bits (10 bits per byte), or False if just the start bits (9 bits per byte).

  # Error handling configuration
  exit_on_start_bit_error = False # Exit if start bit is not 0.
  check_stop_bit = False          # Perform stop bit check (or just skip it).
  exit_on_stop_bit_error = True  # Exit if stop bit is not 1.

  # Image output configuration
  save_frames = False          # Set to true to save all frames (since the start decoding time).
  save_dots = False            # Set to true to save all dots (since the start of bitstream is detected).



# Global class to make subs share variables.
class G:
  stream_found = False            # True if we have found the start of the bitstream (i.e. first start code).
  byte_counter = 0                # Decoded bytes counter.
  prev_byte = -1                  # Used to check for termination (i.e. two zero bytes).
  status = ""                     # Current read status.
  
  # Current byte being read:
  cur_byte = 0                # Current byte being read
  await_start = True          # Waiting for the start bit (0).
  bit_number = 0              # Current bit of current byte being read (0 to 7). Note: Bitstream is LSb first.
  await_stop = False          # Waiting for the stop bit (1).

  # Output files.
  file_out_raw_txt = None
  file_out_txt = None
  file_out_bin = None


def main():
  # Set this to the path of the video file to read.
  vidcap = cv2.VideoCapture("4cb.mp4")
  success, image = vidcap.read()

  if (not success):
    print("Failed to open video file.")
    exit()


  # Create output dirs
  if (Cfg.save_frames):
    path_output_frames = "output_frames"
    os.makedirs(path_output_frames, exist_ok = True)
  if (Cfg.save_dots):
    path_output_dots = "output_dots"
    os.makedirs(path_output_dots, exist_ok = True)


  # Open files
  G.file_out_raw_txt = open('output_raw.txt', 'w') # This contains the raw bitstream (since the first detected start bit).
  G.file_out_txt = open('output.txt', 'w')         # This contains the decoded bytes in text mode (debugging).
  G.file_out_bin = open('output.bin', 'wb')        # This contains the decoded bytes (actual output).

  # IMPORTANT: This is where "the dot" is found on screen.

  x = 579  # Set this to the x-coordinate of the center of "the dot".
  y = 440  # Set this to the y-coordinate of the center of "the dot" (should be an number even).

  frame_num = 0


  # Read frame loop.
  while success:
    # Read aframe.
    success, image = vidcap.read()
    if (not success):
      print(f"Failed to read frame {frame_num}") # This is normal at end of stream.
      exit()


    ts = frame_num * (1/25 if Cfg.interlaced else 1/50)
    skip_frame = False
    skip_str = ""
    if (ts < Cfg.start_at_ts):
      skip_frame = True
      skip_str = "SKIPPED"

    print(f"Read frame {frame_num}, ts = {ts/60:.0f}:{ts%60:02.3f} {skip_str}")

    # Check if we need to start this frame (i.e. we start decoding at a later timestamp).
    if (skip_frame):
      frame_num += 1
      continue

    # Do we need to save this frame (for debugging) ?
    if (Cfg.save_frames):
      success = cv2.imwrite(os.path.join(path_output_frames, f"frame_{frame_num}.jpg"), image)     # Save current frame as JPEG to temp drive.
      if (not success):
        print(f"Failed to save frame_{frame_num} image")
        exit

    pixel_RGB = image[y, x] # NOTE: OpenCV addresses [row, col].
    bit = pixel_RGB[1] > 128 # Use green level.
    on_next_bit_recv(bit)

    if (Cfg.save_dots):
      if (G.stream_found):
        dot_size = 16  # dot size in pixels
        top_left_x = x - (dot_size >> 1)
        top_left_y = y - (dot_size >> 1)
        # Extract and save "the dot" image.
        dot_image = image[top_left_y : (top_left_y + dot_size), top_left_x : (top_left_x + dot_size)]
        success = cv2.imwrite(os.path.join(path_output_dots, f"dot_{frame_num}.png"), dot_image)
        if (not success):
          print(f"Failed to save dot_{frame_num} image")
          exit


    # If interlaced, decode two bits per frame.
    if (Cfg.interlaced):
      pixel_RGB = image[y + 1, x] # NOTE: OpenCV addresses [row, col].
      bit = pixel_RGB[1] > 128 # Use green level.
      on_next_bit_recv(bit)

    # Decode next frame
    frame_num += 1

  #end of main
  exit()



# This function is called whenever a bit has been received and must be processed.
def on_next_bit_recv(bit):
  if (G.await_start): G.status = "await_start"
  elif (G.await_stop): G.status = "await_stop"
  else: G.status = f"read [{G.byte_counter:08X}] bit {G.bit_number}"

  G.file_out_raw_txt.write(f"{bit:X}") # Write raw bit.
  G.file_out_raw_txt.flush()
  
  # Waiting for a start bit ?
  if (G.await_start):
    if (bit):
      # Received a 1.
      if (G.prev_byte < 0):
        return # Ok. Still waiting for the start of the stream.
      else:
        # This is not the start of the stream, then this should be a 0 bit.
        if (Cfg.exit_on_start_bit_error):
          print(f"  ***** ERROR: Read a 1 but expecting a start bit (0).")
          exit()
        else:
          # Ignore heading 1 bits until we find a start bit.
          print(f"  ***** WARNING: Ignoring heading 1 bit");
          return
    else:
      if (not G.stream_found):
        print(f"  ------------- FOUND START OF STREAM -------------")
        G.stream_found = True # We found the stream.
        G.file_out_raw_txt.write(f"\n{bit:X}")        # Line feed for readability, then replicate this start bit.
      G.await_start = False # Ready to receive bits
    return
  

  # Waiting for a stop bit ?
  if (G.await_stop):
    if (Cfg.check_stop_bit and not bit):
      print(f"  ***** ERROR: Read a 0 but expecting stop bit (1).")
      if (Cfg.exit_on_stop_bit_error): exit()
    else:
      if (bit):
        # Current byte completed.
        on_cur_byte_completed()
      else:
        # Ignore trailing 0 bits until we find a stop bit.
        print(f"  ***** WARNING: Ignoring trailing 0 bit");
    return


  # Read a bit of the current byte.
  if (bit): G.cur_byte = G.cur_byte | 0x80
  G.bit_number += 1
  if (G.bit_number == 8):
    # Read 8 bits.
    if (Cfg.stop_bit_used):
      # We now need to wait for the stop bit.
      G.await_stop = True
    else:
      # There is no stop bit, so byte is complete.
      on_cur_byte_completed()
      return
  else:
    # Shift current byte right.
    G.cur_byte >>= 1

  print(f"  Read bit = {bit}. Status = {G.status}")
  return



# This function is called whenever the current byte has been completed.
def on_cur_byte_completed():
  print(f"  >>>>>>>>>>>>>>>>>>>>>>>>>>> READ BYTE: {G.cur_byte:08b}")
  G.file_out_txt.write(f"{G.byte_counter:08X}: {G.cur_byte:08b} = 0x{G.cur_byte:02X}\n")
  G.file_out_bin.write(G.cur_byte.to_bytes(1, byteorder='big')) # Whatever, Python.
  G.byte_counter += 1
  if (G.prev_byte == 0) and (G.cur_byte == 0):
    # End of stream.
    print(f"  ------------- FOUND END OF STREAM -------------")
    G.file_out_raw_txt.close()
    G.file_out_txt.close()
    G.file_out_bin.close()
    exit()
  # Prepare for next byte.
  G.await_stop = False
  G.await_start = True
  G.prev_byte = G.cur_byte
  G.bit_number = 0
  G.cur_byte = 0
  return

# Call the main function
if __name__ == "__main__":
    main()

