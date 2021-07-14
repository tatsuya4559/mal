include BatString

let replace_all ~sub ~by s =
  BatString.nreplace ~str:s ~sub ~by
