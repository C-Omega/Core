namespace C_Omega
module Tree =
    type 'a Tree  =
        | Root   of 'a Tree seq
        | Branch of 'a Tree seq
        | Leaf   of 'a
    type IndexedTree<'Index, 'Value> =
        | Root   of          IndexedTree<'Index, 'Value> seq
        | Branch of 'Index * IndexedTree<'Index, 'Value> seq
        | Leaf   of 'Index * 'Value
        member x.Index = 
            match x with    
            | Branch (i,_)
            | Leaf   (i,_) -> i
            | Root      _  -> failwith "Roots do not have an index"
        member x.Children =
            match x with
            | Root      c
            | Branch (_,c) -> c
            | Leaf    _    -> failwith "Leaves do not have children"
        static member ofDirectory (path : string) =
            let rec inner p = 
                System.IO.Directory.EnumerateDirectories(path)
                |> Seq.map inner
                |> Seq.append (System.IO.Directory.EnumerateFiles(path) |> Seq.map (fun s -> Leaf(s, ())))
                |> fun i -> Branch(p,i)
            Root((inner path).Children)