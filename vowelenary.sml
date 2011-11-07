(*  vowelenary.sml
 *
 *  A vowel counting system. Base n, where n is the number of distinct vowels
 *  in the alphabet. In this case, there are 5 vowels.
 *  A number is made of the digits that are interspersed with place-holder
 *  consonants, whose sequences are undefined (ranging from being random to
 *  spelling out the names of those numbers in that language -- these latter
 *  names may have to be neologisms).
 *
 *  In this case:
 *  A   E   I   O   U
 *  1   2   3   0   4
 *
 *  Therefore it is equivalent to a base 5. Except that it is easy to
 *  read out loud.
 * 
 *  *)

signature CONSONANTS = sig
    type generator
    val newGenerator : int -> generator
    val closeGenerator : generator -> unit
    val withGenerator : int -> (generator -> 'a) -> 'a
    val nextString : generator -> int -> (string * string) * generator
end

signature VOWELENARY = sig
    val toString : int -> string
    val toInt : string -> int
end

structure RandomConsonants : CONSONANTS = struct
    structure TIO = TextIO

    datatype generator = Gen of int * TIO.instream

    val consonants = Vector.fromList (String.explode "bcdfghjklmnprstvwyz")
    
    fun newGenerator x =
        let
            val rnd = TIO.openIn "/dev/random"
        in
            Gen (x, rnd)
        end

    and closeGenerator (Gen (_, rnd)) = TIO.closeIn rnd

    and nextString (gen as (Gen (_, rnd))) dig =
        let
            val g = case TIO.input1 rnd of
                SOME pos  => [Vector.sub (consonants,
                              (Char.ord pos) mod (Vector.length consonants))]
            |   NONE      => [#"x"]
        in
            ((String.implode g, ""), gen)
        end

    and withGenerator x f =
        let
            val g = newGenerator x
        in
            f g before closeGenerator g
        end
end

functor Vowelenary (C:CONSONANTS) : VOWELENARY = struct
    val cns = Vector.fromList (String.explode "bcdfghjklmnpqrstvwxyz")
    local
        fun to_i []      d = d
        |   to_i (x::xs) d =  if Vector.exists (fn y => y = x) cns then
                to_i xs d else to_i xs ((d * 5) + (case Char.toLower x of
                            #"a"    => 1
                        |   #"e"    => 2
                        |   #"i"    => 3
                        |   #"u"    => 4
                        |   _       => 0))
    in
        fun toInt str = to_i (String.explode str) 0
    end

    local
        val vwls = Vector.fromList (String.explode "oaeiu")
        fun to_s g lst x =
            let
                val mden         = x mod 5
                val ((p, n), g') = C.nextString g mden
                val nxt          = ([p,
                                    Char.toString (Vector.sub (vwls, mden)),
                                    n] @ lst)
            in
                if x < 5 then String.concat nxt else to_s g' nxt (x div 5)
            end
                
    in
        fun toString x = C.withGenerator x (fn g => to_s g [] x)
    end
end

structure CV = Vowelenary(RandomConsonants)
