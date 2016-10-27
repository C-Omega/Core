namespace C_Omega
open ArraySliceImprovement
open System.Collections.Generic
module Store =
    ///A common interface for stores
    type IStore<'Index, 'Value> =
        interface
            abstract get       : 'Index -> 'Value option
            abstract Item      : 'Index -> 'Value
            ///Nothing MUST happen if the index is invalid
            abstract set       : 'Index -> 'Value -> unit
            ///Nothing MUST happen if the index already exists
            abstract add       : 'Index -> 'Value -> unit
            abstract search    : 'Value -> seq<'Index>
            abstract find      :('Index -> 'Value -> bool)  -> ('Index * 'Value) option
            abstract findall   :('Index -> 'Value -> bool)  -> ('Index * 'Value) seq
            abstract removeall :('Index -> 'Value -> bool)  -> unit
            abstract setall    :('Index -> 'Value -> 'Value option) -> unit
            abstract remove    : 'Index -> unit
            abstract choose    :('Index -> 'Value -> obj option) -> obj seq
            abstract seq       : unit   ->('Index *  'Value) seq
            abstract indices   : unit   -> 'Index seq
        end
    let map (store : IStore<'a, 'b>) (indexMap : Mapper<'a, 'c>) (valueMap : Mapper<'b, 'd>) : IStore<'c, 'd> =
        let im = indexMap.map
        let iu = indexMap.unmap
        let vm = valueMap.map
        let vu = valueMap.unmap
        {new IStore<'c, 'd> with
            member x.Item i      = i |> iu |> store.Item   |> vm
            member x.get i       = i |> iu |> store.get    |> Option.map vm
            member x.set i v     = store.set (iu i) (vu v)
            member x.add i v     = (store.add (iu i) (vu v))
            member x.search i    = i |> vu |> store.search |> Seq.map im
            member x.find f      = store.find      (fun i v -> f (im i) (vm v)) |> Option.map (fun (i, v) -> im i, vm v)
            member x.findall f   = store.findall   (fun i v -> f (im i) (vm v)) |> Seq.map (fun (i, v) -> im i, vm v)
            member x.removeall f = store.removeall (fun i v -> f (im i) (vm v))
            member x.setall f    = store.setall    (fun i v -> f (im i) (vm v) |> Option.map vu)
            member x.remove i    = store.remove    (iu i)
            member x.choose f    = store.choose    (fun i v -> f (im i) (vm v))
            member x.seq()       = store.seq()     |> Seq.map (fun (i,v) -> im i, vm v)
            member x.indices()   = store.indices() |> Seq.map (fun i   -> im i      )
        }
    let _dict_store (d:Dictionary<'a, 'b>) : IStore<'a, 'b> =
        {new IStore<'a, 'b> with
            member x.Item i      = d.Item i
            member x.get i       = i |>  d.TryGetValue |> function | false, _ -> None | _, a -> Some a
            member x.set i j     = d.[i] <- j
            member x.add i j     = try d.Add(i, j) with |_ -> ()
            member x.search i    = Seq.choose (function |KeyValue(a,b) when b = i -> Some a    |_ -> None) d
            member x.find f      = Seq.tryPick(function |KeyValue(a,b) when f a b -> Some(a,b) |_ -> None) d
            member x.findall f   = Seq.choose (function |KeyValue(a,b) when f a b -> Some(a,b) |_ -> None) d
            member x.removeall f = Seq.iter   (function |KeyValue(a,b) when f a b -> d.Remove a |> ignore |_ -> ()) d
            member x.setall f    = Seq.iter   (function |KeyValue(a,b) ->   f a b |> Option.iter (fun c -> d.[a] <- c)) d
            member x.remove i    = d.Remove i |> ignore
            member x.choose f    = Seq.choose (function |KeyValue(a,b) ->   f a b) d
            member x.seq()       = Seq.map    (function |KeyValue(a,b) ->     a,b) d
            member x.indices() = d.Keys |> Seq.cast
        }
    open System.IO
    let _dir_store (path : string) =
        let r i =
            try File.ReadAllBytes (path + "/" + i) |> Some
            with | _ -> None
        {new IStore<string, byte[]> with
            member x.Item i      = (r i).Value
            member x.get i       = r i
            member x.set i j     = if File.Exists i then File.WriteAllBytes(path+"/"+i,j)
            member x.add i j     = if not(File.Exists i) then File.WriteAllBytes(path+"/"+i,j)
            member x.search i    = Directory.EnumerateFiles path |> Seq.filter (fun s -> File.ReadAllBytes s = i)//if File.Exists (path+"/"+i) then Seq.singleton (File.ReadAllText i) else Seq.empty
            member x.find f      = path |> Directory.EnumerateFiles |> Seq.tryPick (fun s -> r s |> function |None -> None |Some b -> if f s b then Some(s, b) else None)
            member x.findall f   = path |> Directory.EnumerateFiles |> Seq.choose  (fun s -> r s |> function |None -> None |Some b -> if f s b then Some(s, b) else None)
            member x.removeall f = path |> Directory.EnumerateFiles |> Seq.iter    (fun s -> r s |> function |None -> ()   |Some b -> if f s b then File.Delete s       )
            member x.setall f    = path |> Directory.EnumerateFiles |> Seq.iter    (fun s -> r s |> function |None -> ()   |Some b -> f s b |> Option.iter (fun b' -> File.WriteAllBytes(s, b')))
            member x.remove i    = try File.Delete i with |_ -> ()
            member x.choose f    = path |> Directory.EnumerateFiles |> Seq.choose  (fun s -> r s |> function |None -> None |Some b -> f s b)
            member x.seq ()      = path |> Directory.EnumerateFiles |> Seq.map     (fun s -> s, File.ReadAllBytes s)
            member x.indices()   = path |> Directory.EnumerateFiles
        }
    (*
    type FSNode =
        | File of path : string
        | Directory of path : string * store : Store<string,FSNode>
    let rec _fs_store (root : string) =
        {
            member x.Item      = 
                let p = i+"/"+root
                if System.IO.Directory.Exists p then Directory(p, _fs_store p)
                elif System.IO.File.Exists p then File p
                else raise(System.IO.FileNotFoundException("The file system node could not be found.",p))
            member x.get       = fun i   ->
                let p = i+"/"+root
                if System.IO.Directory.Exists p then Directory(p, _fs_store p) |> Some
                elif System.IO.File.Exists p then File p |> Some
                else None
            member x.add       = fun i j ->


        }
        *)