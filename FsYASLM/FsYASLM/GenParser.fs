module GenParser 

open FSharpx.Prelude

module GpInterface = 
    type IMonad<'a> = 
        abstract member Bind : ('a -> IMonad<'b>) -> IMonad<'b>
        //abstract static Return : 'a
    let mbind (m : #IMonad<'a>) f = m.Bind f
    //let mreturn (x : 'a) : IMonad<'a> = 

module GpInline = 
    [<AllowNullLiteral>]
    type Computation<'s, 'a, 'err> (myF: ('s -> Choice<'a * 's, 'err> )) = 
        member x.Invoke = myF
        static member Bind(m : Computation<'s, 'a, 'err>, (f: 'a -> Computation<'s, 'b, 'err>) ) : Computation<'s, 'b, 'err> = 
            new Computation<'s, 'b, 'err> (fun s -> 
                match m.Invoke(s) with
                | Choice1Of2(res1, s1) -> 
                    let m2 = f res1
                    m2.Invoke(s1)
                | Choice2Of2 x -> Choice2Of2 x )
                    
        static member Returnm(x) = Computation<_,_,_>(fun s -> Choice1Of2(x, s) )
    
    type GenParserStateData<'input> = {
        input     : 'input list;
        pos       : int;
        storedPos : int
    }
    type ParserState<'input, 's> = GenParserStateData<'input> * 's
    type GenParserErrorData<'input, 's> = {
        msg   : string;
        state : ParserState<'input, 's>
    }
    type GenParser<'input, 's, 'a> = Computation<ParserState<'input, 's>, 'a, GenParserErrorData<'input, 's> >
    
    let inline mbind (m : ^ma) f = (^ma: (static member Bind : ^ma -> ('a -> 'mb) -> 'mb) (m, f) )
    let inline mreturn (x : 'a) : ^ma = (^ma: (static member Returnm : 'a -> ^ma) (x) )
        
    let inline (>>=) m f = mbind m f
    let inline (>>) a b = a >>= (fun _ -> b)
    let inline liftM f m = m >>= (fun x -> f x |> mreturn )
    let inline (<@>) f m = liftM f m
    let inline (<@@>) f m1 = fun m2 -> m1 >>= (fun a -> m2 >>= (fun b -> mreturn <| f (a, b) ) ) 
    let inline (<*>) f a = f >>= (fun f' -> a >>= (fun a' -> mreturn <| f' a' ))
    
    
    let inline runState (m : Computation<'s, 'a, 'e>) (init) cont econt = 
        match m.Invoke(init) with
        | Choice1Of2 x -> cont x
        | Choice2Of2 x -> econt x
          
    //let (<&>) = liftM
    //let inline mfail<'s, 'a, 'e> (e: 'err) = Computation<'s,'a,'err>(fun _ -> Failure e )
    let inline private getState<'s,'err>   = Computation<'s, 's, 'err>(fun (s : 's) -> Choice1Of2(s, s) )
    let inline private putState<'s,'err> (s : 's) = Computation<'s, unit, 'err>(fun _ -> Choice1Of2((), s) )
    
    let inline private modify<'s, 'err> (f: 's -> 's ) : Computation<'s, unit, 'err> = 
        getState >>= (fun (st : 's) -> putState (f st) )
    let inline private gets f = getState >>= (fun st -> mreturn (f st) )
    let inline eval m s = m s |> fst
    let inline exec m s = m s |> snd
    let inline nopState<'s,'err> = Computation<'s, unit, 'err>(fun s -> Choice1Of2((), s) )

    type MonadBuilder() =
        member inline this.Return(x) = mreturn x
        member this.ReturnFrom(m) = m
        member inline this.Bind(m, f) = m >>= f
        member inline this.Delay(f) =  f()


    
    let genp<'input, 's, 'a> = new MonadBuilder()
    
   
    let inline preturn x : GenParser<'input, 's, 'a> = mreturn x
    let inline pfail e : GenParser<'input, 's, 'a> = GenParser<_,_,_>(fun _ -> Choice2Of2 e )//AbortableState<'s,'a,'err>(fun _ -> Failure e )
    let inline pfailwith (msg: string) = getState >>= (fun st -> pfail ({msg = msg; state = st}))
    
    let runParser p (inp: 'input list) (st: 's) = 
        runState p ({ input = inp; pos = 0; storedPos = 0 }, st)

    let inline gpModify f = modify (fun (inp, s) -> (inp, f s) )
    let gpGetState<'input, 's> : GenParser<'input, 's, 's> =
         getState >>= (fun (_ : GenParserStateData<'input>, s: 's) -> mreturn s)
    let inline (<|>) (p1: GenParser<'input, 's, 'a>) (p2: GenParser<'input, 's, 'a>) : GenParser<'input, 's, 'a> = genp {
        let! st = getState
        return! runState p1 st (fun (value, st1) -> putState st1 >> mreturn value ) (konst p2)
    }
    let inline gpGets f = gets (fun (inp, s) -> (inp, f s) )
    //let consume<'input, 's, 'a> = modify (fun (inp, s) -> ({ inp with input = inp.input |> List.tail; }, s) )
    let inline consumen n : GenParser<'input, 's, unit> = modify (fun (inp, s) -> ( { inp with input = Seq.skip n inp.input |> Seq.toList; } , s) )
    let nextLexem<'input, 's> : GenParser<'input, 's, Option<'input> > = 
        gets (fun s -> 
            let l = (fst s).input in if List.isEmpty l then None else Some <| List.head l )
    let inline matchNext (fpred : 'input -> bool) : GenParser<'input, 's, 'input> = 
        nextLexem >>= (function 
                        | Some(lx) ->  if fpred lx then consumen 1 >> preturn lx else pfailwith "not match"
                        | None -> pfailwith "empty")


    let manyOrNone (parser : GenParser<'input, 's, 'a>) : GenParser<'input, 's, 'a list> =
        let rec manyOrNone' (acc: 'a list) : GenParser<'input, 's, 'a list> = 
            parser >>= (manyOrNone' << ( (flip << curry) List.Cons acc) ) <|> preturn acc
        in liftM (List.rev) (manyOrNone' [])

    
    let inline many parser = liftM (curry List.Cons) parser <*> manyOrNone parser

    let inline separatedBy parser separator = liftM (curry List.Cons) parser <*> manyOrNone (separator >> parser)
    let inline (-|-) p s = separatedBy p s

    let inline wrappedBy parser (beg, close) = beg >> parser >>= (fun r -> close >> mreturn r)
    let inline (/--/) p b = wrappedBy p b

    let inline followedBy parser by = parser >>= (fun r -> by >> mreturn r)
    let inline (|-->) p b = followedBy p b

    let inline oneOrNone parser = liftM Some parser <|> preturn None
    
    let future r = genp { 
        do! nopState
        return! !r
    }















module GpBase =        

    type AbortableState<'s, 'a> = ('s -> ('a * 's) option)

    let runState (m : AbortableState<'s, 'a>) (init : 's) = 
        m init
    let mreturn x = (fun s -> Some(x, s) )
    let (>>=) m f = (fun s -> Option.bind (fun (res1, s') -> runState (f res1) s') (runState m s) )
    let (>>) m f = m >>= (fun _ -> f)
          
    
    let liftM (f : 'a -> 'r) m
        = m >>= (fun x -> f x |> mreturn )
    //let (<&>) = liftM
    let (<*>) f a = f >>= (fun f' -> a >>= (fun a' -> mreturn <| f' a' ))
    let mfail = (fun s -> None)
    let private getState = fun s -> Some(s,s)
    let private putState s = fun _ -> Some((),s)
    let private modify f = getState >>= (fun st -> putState (f st) )
    let private gets f = getState >>= (fun st -> mreturn (f st) )
    let eval m s = m s |> fst
    let exec m s = m s |> snd
    let empty = fun s -> ((), s)

    type AbortableStateBuilder() =
        member this.Return(x) = mreturn x
        member this.ReturnFrom(m) = m
        member this.Bind(m, f) = m >>= f


    
    let genp = new AbortableStateBuilder()
    type GenParserState<'input> = {
        input     : 'input list;
        pos       : int;
        storedPos : int
    }
    type ParserState<'input, 's> = GenParserState<'input> * 's
    type GenParser<'input, 's, 'a> = AbortableState<ParserState<'input, 's>, 'a>
        
    let (<|>) (p1: GenParser<'input, 's, 'a>) (p2: GenParser<'input, 's, 'a>) = genp {
        let! st = getState
        let res = runState p1 st
        return! match res with
                    | Some (value, st1) -> 
                        genp { 
                            do! putState st1
                            return value 
                        }
                    | None -> p2
    }
    
    let gpModify f = modify (fun (inp, s) -> (inp, f s) )
    let gpGets f = gets (fun (inp, s) -> (inp, f s) )
    //let consume<'input, 's, 'a> = modify (fun (inp, s) -> ({ inp with input = inp.input |> List.tail; }, s) )
    let consumen n = modify (fun (inp, s) -> ( { inp with input = Seq.skip n inp.input |> Seq.toList; } , s) )

    let matchNextLexem (fpred : 'input -> bool) : GenParser<'input, _, 'input> = genp {
        let! (inp, _) = getState
        //if (length $ psInput st) < 1 then failwith "matchNextLexem list finished unexpected" else return ()
        let lx = List.head inp.input
        return! if fpred lx then consumen 1 >> mreturn lx else mfail
    }

    let manyOrNone (parser : GenParser<'input, 's, 'a>) : GenParser<'input, 's, 'a list> =
        let rec manyOrNone' (acc: 'a list) = parser >>= (manyOrNone' << ( (flip << curry) List.Cons acc) ) <|> mreturn acc
        in liftM (List.rev) (manyOrNone' [])

    
    let many parser = liftM (curry List.Cons) parser <*> manyOrNone parser

    let separatedBy parser separator = liftM (curry List.Cons) parser <*> manyOrNone (separator >> parser)

    let wrappedBy parser (beg, close) = beg >> parser >>= (fun r -> close >> mreturn r)

    let followedBy parser by = parser >>= (fun r -> by >> mreturn r)

    let oneOrNone parser = liftM Some parser <|> mreturn None

    