module GenParser 

open FSharpx.Prelude

module GpInterface = 
    type IMonad<'a> = 
        abstract member Bind : ('a -> IMonad<'b>) -> IMonad<'b>
        //abstract static Return : 'a
    let mbind (m : #IMonad<'a>) f = m.Bind f
    //let mreturn (x : 'a) : IMonad<'a> = 

module GpInline = 
    type AbortableState<'s, 'a> (myF: ('s -> ('a * 's) option)) = 
        member x.Invoke = myF
        static member RunState (m : AbortableState<_,_>, init) = m.Invoke(init)
        static member Bind(m, f) = 
            AbortableState<_,_>(fun s -> 
                Option.bind (fun (res1, s') -> 
                    AbortableState<_,_>.RunState (f res1, s')) (AbortableState<_,_>.RunState (m, s) ) )
        static member Returnm(x) = AbortableState<_,_>(fun s -> Some(x, s) )
    
    
    let inline mbind (m : ^ma) f = (^ma: (static member Bind : ^ma -> ('a -> 'mb) -> 'mb) (m, f) )
    let inline mreturn (x : 'a) : ^ma = (^ma: (static member Returnm : 'a -> ^ma) (x) )
        
    let inline (>>=) m f = mbind m f
    let inline (>>) a b = a >>= (fun _ -> b)
    let inline liftM f m = m >>= (fun x -> f x |> mreturn )
    let inline (<@>) f m = liftM f m
    let inline (<*>) f a = f >>= (fun f' -> a >>= (fun a' -> mreturn <| f' a' ))
    
    
    let runState (m : AbortableState<_, _>) (init) = 
        AbortableState<_,_>.RunState(m, init)
          
    //let (<&>) = liftM
    let inline mfail<'s,'a> = AbortableState<'s,'a>(fun s -> None)
    let inline private getState<'s> = AbortableState<'s,'s>(fun (s : 's) -> Some(s, s) )
    let inline private putState s = AbortableState<_,_>(fun _ -> Some((), s) )
    let inline private modify (f ) = getState >>= (fun (st : 's) -> putState (f st) )
    let inline private gets f = getState >>= (fun st -> mreturn (f st) )
    let inline eval m s = m s |> fst
    let inline exec m s = m s |> snd

    type MonadBuilder() =
        member inline this.Return(x) = mreturn x
        member this.ReturnFrom(m) = m
        member inline this.Bind(m, f) = m >>= f


    
    let genp<'input, 's, 'a> = new MonadBuilder()
    type GenParserStateData<'input> = {
        input     : 'input list;
        pos       : int;
        storedPos : int
    }
    type ParserState<'input, 's> = GenParserStateData<'input> * 's
    type GenParser<'input, 's, 'a> = AbortableState<ParserState<'input, 's>, 'a>
    let inline preturn x : GenParser<'input, 's, 'a> = mreturn x
    let inline pfail<'input, 's, 'a> : GenParser<'input, 's, 'a> = mfail<ParserState<'input, 's>, 'a>
    
    let runParser p (inp: 'input list) (st: 's) = 
        runState p ({ input = inp; pos = 0; storedPos = 0 }, st)

    let inline (<|>) (p1: GenParser<'input, 's, 'a>) (p2: GenParser<'input, 's, 'a>) : GenParser<'input, 's, 'a> = genp {
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
    
    let inline gpModify f = modify (fun (inp, s) -> (inp, f s) )
    let inline gpGets f = gets (fun (inp, s) -> (inp, f s) )
    //let consume<'input, 's, 'a> = modify (fun (inp, s) -> ({ inp with input = inp.input |> List.tail; }, s) )
    let inline consumen n : GenParser<'input, 's, unit> = modify (fun (inp, s) -> ( { inp with input = Seq.skip n inp.input |> Seq.toList; } , s) )
    let nextLexem<'input, 's> : GenParser<'input, 's, Option<'input> > = 
        gets (fun s -> 
            let l = (fst s).input in if List.isEmpty l then None else Some <| List.head l )
    let inline matchNext (fpred : 'input -> bool) : GenParser<'input, 's, 'input> = 
        nextLexem >>= (function 
                        | Some(lx) ->  if fpred lx then consumen 1 >> preturn lx else pfail 
                        | None -> pfail)


    let manyOrNone (parser : GenParser<'input, 's, 'a>) : GenParser<'input, 's, 'a list> =
        let rec manyOrNone' (acc: 'a list) : GenParser<'input, 's, 'a list> = 
            parser >>= (manyOrNone' << ( (flip << curry) List.Cons acc) ) <|> preturn acc
        in liftM (List.rev) (manyOrNone' [])

    
    let inline many parser = liftM (curry List.Cons) parser <*> manyOrNone parser

    let inline separatedBy parser separator = liftM (curry List.Cons) parser <*> manyOrNone (separator >> parser)

    let inline wrappedBy parser (beg, close) = beg >> parser >>= (fun r -> close >> mreturn r)

    let inline followedBy parser by = parser >>= (fun r -> by >> mreturn r)

    let inline oneOrNone parser = liftM Some parser <|> preturn None



















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

    