signature ERRORMSG =
sig
    val anyErrors: bool ref
    val fileName: string ref
    val lin: int ref
    val col: int list ref
    val sourceStream: TextIO.instream ref
    val error: int -> string -> unit
    val lexLog: int * string -> unit
    val yaccLog: string -> unit
    val debug: bool
    exception Error
    val impossiable: string -> 'a (* raises Errors *)
    val reset: unit -> unit
end

structure ErrorMsg:ERRORMSG =
struct
    val anyErrors = ref false
    val fileName = ref ""
    val lin = ref 1
    val col = ref [1]
    val sourceStream = ref TextIO.stdIn
    val debug = false

    fun reset() = (anyErrors:=false;
		fileName:="";
		lin:=1;
		col:=[1];
		sourceStream:=TextIO.stdIn)
    
    exception Error

    fun error pos (msg:string) = 
        let fun look(a::rest, n) = 
                if a < pos then 
                    app print [":", Int.toString n, ".", Int.toString (pos-a)]
                else look(rest, n-1)
            | look _ = print ":0.0"
        in
            anyErrors := true;
            print (!fileName);
            look(!col, !lin);
            print ":";
            print msg;
            print "\n"
        end

    fun lexLog(pos, msg) =
        if debug then
            let fun look(a::rest, n) = 
                if a < pos then 
                    app print [":", Int.toString n, ".", Int.toString (pos-a)]
                else look(rest, n-1)
            | look _ = print ":0.0"
            in
                print (!fileName);
                look(!col, !lin);
                print ":token";
                print ":";
                print msg;
                print "\n"
            end
        else
            ()

    fun yaccLog(msg) = 
        if debug then(
            print "grammar";
            print ":";
            print msg;
            print "\n"
        )
        else
            ()

    fun impossiable msg = 
        (
            app print ["Error: Compiler bug: ", msg, "\n"];
            TextIO.flushOut TextIO.stdOut;
            raise Error
        )
end