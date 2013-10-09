module Strmap
    open System.Collections.Generic
 
    type strmap() = class
        let mutable root = new Dictionary<string, obj>()
        let dso (s:string) (a:obj) = (a :?> Dictionary<string, obj>).[s]
        let upsent (a:obj) (s:string) = (a :?> Dictionary<string, obj>).ContainsKey(s) |> not
        let conv (a:obj) = a :?> Dictionary<string, obj>
        member x.Item 
            with get (y:string) = root.[y] :?> string
            and set (y:string) (z:string) = root.[y] <- z
        member x.Item 
            with get (y:string*string) = 
                let a,b = y 
                if upsent root a then root.[a] <- new Dictionary<string, obj>() 
                if upsent root.[a] b then null else (root.[a] |> conv).[b] :?> string
            and set (y:string*string) (z:string) = 
                let a,b = y 
                if upsent root a then root.[a] <- new Dictionary<string, obj>() 
                (conv root.[a]).[b] <- z
        member x.Item 
            with get (y:string*string*string) = 
                let a,b,c = y 
                if upsent root a then root.[a] <- new Dictionary<string, obj>() 
                if upsent root.[a] b then (a |> conv).[b] <- new Dictionary<string, obj>() 
                if upsent (root.[a] |> dso b |> conv) c then null else (root.[a] |> dso b |> conv).[c] :?> string
            and set (y:string*string*string) (z:string) = 
                let a,b,c = y 
                if upsent root a then root.[a] <- new Dictionary<string, obj>() 
                if upsent root.[a] b then (a |> conv).[b] <- new Dictionary<string, obj>()
                ((root.[a] |> conv).[b] |> conv).[c] <- z 
    end
