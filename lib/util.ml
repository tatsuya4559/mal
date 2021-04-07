module Option_ops = struct
  let ( let* ) x f = Option.bind x f
  let return = Option.some
end

module File = struct
  let read_all filename =
    BatFile.with_file_in filename (fun input ->
      BatIO.read_all input)
end

