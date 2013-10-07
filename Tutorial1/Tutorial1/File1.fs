let id x = x
let factor x = 
    let mutable mem = [1; 1]
    (1, 2) |> id

    (*let fn x =
         if mem.Length > x then 
            mem.Item x
         else
            let k = mem.Length
            let last = mem.Item(mem.Length - 1)
            for i = k to x do 
                mem <- mem @ [mem.Item(mem.Length - 1) * k]
            mem.Item(mem.Length - 1) 
    fn*)
    
let fc = factor 5

