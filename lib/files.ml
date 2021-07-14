let read_all filename =
  BatFile.with_file_in filename (fun input ->
    BatIO.read_all input)
