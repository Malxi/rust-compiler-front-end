signature CONVERT =
sig
    datatype encoding = UTF8 | UTF32
    exception EncodingError of string
    val toChar : string -> char
    val decode : string * encoding -> int list
    val decodeChar : string * encoding -> int
end

structure Convert : CONVERT =
struct
    datatype encoding = UTF8 | UTF32
    exception EncodingError of string

    fun toChar(text:string) = 
    let
        val cc = String.explode (text)
        val c = hd(cc)
    in
        (* app print ["char literal: ", Char.toCString c, "\n"]; *)
        c
    end

    fun toInteger(text:string) = case LargeInt.fromString(text) of 
                                SOME v => v
                                | _ => 0
    
    fun toFloat(text:string) = case Real.fromString(text) of 
                                SOME v => v
                                | _ => 0.0

    fun escape(text:string, pos:int) = 
    let
        val chs = String.explode text

        fun hex2dec (nil, v) = v
            | hex2dec (#"a"::t, v) = hex2dec(t, v*16+10)
            | hex2dec (#"A"::t, v) = hex2dec(t, v*16+10)
            | hex2dec (#"b"::t, v) = hex2dec(t, v*16+11)
            | hex2dec (#"B"::t, v) = hex2dec(t, v*16+11)
            | hex2dec (#"c"::t, v) = hex2dec(t, v*16+12)
            | hex2dec (#"C"::t, v) = hex2dec(t, v*16+12)
            | hex2dec (#"d"::t, v) = hex2dec(t, v*16+13)
            | hex2dec (#"D"::t, v) = hex2dec(t, v*16+13)
            | hex2dec (#"e"::t, v) = hex2dec(t, v*16+14)
            | hex2dec (#"E"::t, v) = hex2dec(t, v*16+14)
            | hex2dec (#"f"::t, v) = hex2dec(t, v*16+15)
            | hex2dec (#"F"::t, v) = hex2dec(t, v*16+15)
            | hex2dec (h::t, v) = hex2dec(t, v*16+ (ord h) - (ord #"0"))
        
        fun unicode (nil, v) = v
            | unicode (#"{"::t, v) = unicode(t, v)
            | unicode (#"}"::t, v) = unicode(t, v)
            | unicode (#"_"::t, v) = unicode(t, v)
            | unicode (h::t, v) = unicode(t, v*16+ (ord h) - (ord #"0"))
        (* 
            This function convert char literal to a string for Char.fromString.
            However, Char.fromString can not work when unicode point is in ordinal range of the alphabet.
        *)
        fun convert (nil) = Char.ord #"\000"
            | convert (#"x"::t) = hex2dec(t, 0)
            | convert (#"u"::t) = unicode(t, 0)
            | convert (#"n"::t) = Char.ord #"\n"
            | convert (#"r"::t) = Char.ord #"\r"
            | convert (#"t"::t) = Char.ord #"\t"
            | convert (#"0"::t) = Char.ord #"\000"
            | convert (#"\092"::t) = Char.ord #"\092"
            | convert (#"'"::t) = Char.ord #"'"
            | convert (#"\""::t) = Char.ord #"\""
            | convert (h::t) = Char.ord (toChar(implode([#"\092", h])))
    in
        case chs of
        (#"\092"::t) => convert(t)
        | _ => (ErrorMsg.error pos ("illegal escape " ^ text);0)
    end

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
