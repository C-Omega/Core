namespace C_Omega
open ArraySliceImprovement
open System.Collections.Generic
module Store =
    ///A common interface for stores
    type Store<'Index, 'Value> =
        {
            Item      : 'Index -> 'Value
            get       : 'Index -> 'Value option
            set       : 'Index -> 'Value -> unit
            add       : 'Index -> 'Value -> unit
            search    : 'Value -> seq<'Index>
            find      :('Index -> 'Value -> bool)  -> ('Index * 'Value) option
            findall   :('Index -> 'Value -> bool)  -> ('Index * 'Value) seq
            removeall :('Index -> 'Value -> bool)  -> unit
            setall    :('Index -> 'Value -> 'Value option) -> unit
            remove    : 'Index -> unit
            choose    :('Index -> 'Value -> obj option) -> obj seq
            seq       :('Index *  'Value) seq
        }
    let _dict_store (d:Dictionary<'a, 'b>) : Store<'a, 'b> =
        {
            Item      = fun i   -> d.Item i
            get       = d.TryGetValue >> function | false, _ -> None | _, a -> Some a
            set       = fun i j -> d.[i] <- j
            add       = fun i j -> d.Add(i, j)
            search    = fun i   -> Seq.choose (function |KeyValue(a,b) when b = i -> Some a    |_ -> None) d
            find      = fun f   -> Seq.tryPick(function |KeyValue(a,b) when f a b -> Some(a,b) |_ -> None) d
            findall   = fun f   -> Seq.choose (function |KeyValue(a,b) when f a b -> Some(a,b) |_ -> None) d
            removeall = fun f   -> Seq.iter   (function |KeyValue(a,b) when f a b -> d.Remove a |> ignore |_ -> ()) d
            setall    = fun f   -> Seq.iter   (function |KeyValue(a,b) ->   f a b |> Option.iter (fun c -> d.[a] <- c)) d
            remove    = d.Remove >> ignore
            choose    = fun f   -> Seq.choose (function |KeyValue(a,b) ->   f a b) d
            seq       =            Seq.map    (function |KeyValue(a,b) ->     a,b) d
        }