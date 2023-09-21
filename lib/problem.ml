  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

