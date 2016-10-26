namespace C_Omega
open ArraySliceImprovement
module Store =
    ///A common apparently immutable interface for stores
    type Store<'Index, 'Value> =
        {
            get       : 'Index -> 'Value
            set       : 'Index -> 'Value -> Store<'Index, 'Value>
            add       : 'Index -> 'Value -> Store<'Index, 'Value>
            search    : 'Value -> seq<'Index>
            find      :('Index -> 'Value -> bool)  -> ('Index * 'Value) option
            findall   :('Index -> 'Value -> bool)  -> ('Index * 'Value) seq
            removeall :('Index -> 'Value -> bool)  -> unit
            setall    :('Index -> 'Value -> 'Value)-> unit
            remove    : 'Index -> Store<'Index, 'Value>
        }
    