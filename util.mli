val implode : char list -> string
val explode : string -> char list
val dateAndTime : unit -> string
val writeln : out_channel * int * string -> unit 
val write   : out_channel * int * string -> unit 
val makeFileName : string * string -> string
val makeFile : string * string -> out_channel
val interpreterName : string
val zip : 'a list * 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
