type Struct =
  fst: u64 and
  snd: bool 

type Struct = fst: u64 and snd: bool

type Enum =
  fst:
    str: String and
    rec: u64 and 
    heh: u32 or
  snd or
  third or
  fourth

fn main() = (\x -> x) // probably not gonna be how we create unit in the end. will have to look in the future at it
