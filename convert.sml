signature CONVERT =
sig
    datatype encoding = UTF8 | UTF32
    exception EncodingError of string
    val decode : string * encoding -> int list
    val decodeChar : string * encoding -> int
end

structure Convert : CONVERT =
struct
    datatype encoding = UTF8 | UTF32
    exception EncodingError of string
    fun readN(0, v, t) = 
        if v > 0x10FFFF then
            raise EncodingError ("Point ("^Int.toString(v)^") is too large.")
        else v::(next t)
        | readN(_, _, nil) = 
            raise EncodingError ("Unexpected end of string.")
        | readN(n, v, h::t) = 
            let 
                val hn = Char.ord h
                (* continuation byte: 10xx_xxxx in [0x80, 0xC0) *)
                val nv = if hn < 0x80 orelse hn >= 0xC0 then 
                            raise EncodingError ("Invalid continuation byte("^Int.toString(hn)^").")
                        else hn-0x80
            in
                readN(n-1, v*64+nv, t)
            end
    and next(nil) = nil
        | next(h::t) = 
            let
                val hn = Char.ord h
                val (len, value) = 
                    if hn > 0xF4 then raise EncodingError ("Start byte("^Int.toString(hn)^") is too large.")
                    else if hn >= 0xF0 then (4, hn-0xF0)
                    else if hn >= 0xE0 then (3, hn-0xE0)
                    else if hn >= 0xC0 then (2, hn-0xC0)
                    else if hn >= 0x80 then raise EncodingError ("Start byte("^Int.toString(hn)^") is too small.")
                    else (1, hn)
            in
                readN(len-1, value, t)
            end
    fun decode(text, UTF8) = 
        let 
            val chs = String.explode text
        in
            next(chs)
        end
        | decode(text, _) = 
            raise EncodingError ("This encoding is not supported.")
    
    fun decodeChar(text, UTF8) = 
        let 
            val chs = String.explode text
            fun log (nil) = ()
                | log (h::t) = (print(Int.toString(Char.ord(h)));log(t))
        in
            case next(chs) of
                (nil) => 0
                | (h::t) => h
        end
        | decodeChar(text, _) = 
            raise EncodingError ("This encoding is not supported.")
end
