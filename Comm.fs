namespace C_Omega
open ArraySliceImprovement
module Comm = 
    open System.Net
    open Helpers
    open Logger
    exception WrongCommType
    type SendReceive<'a> = 
        {send : 'a -> unit;receive : unit -> 'a}
        member x.Map (m:Mapper<'b,_>) = {send = m.map >> x.send;receive = x.receive >> m.unmap}
        member x.Log (logger:Logger) = {send = x.send >> side (logger.log << (sprintf ">%A")); receive = x.receive >> side (logger.log << (sprintf "<%A"))}
        member x.FlushTo (v:'a) = if Unchecked.equals v (x.receive()) then () else x.FlushTo v
    let loopback<'b>() : SendReceive<'b>= let b = new System.Collections.Generic.Queue<'b>() in {send = b.Enqueue; receive = fun() -> spinuntil(fun() -> b.Count > 0); b.Dequeue()}
    type bsr = SendReceive<byte[]>
    type BufferedSendReceive<'a>(sr:SendReceive<'a>) = 
        let v = new ResizeArray<'a>()
        let s = ref true
        do async{while !s do let x = sr.receive() in lock v (fun() -> v.Add x)}|>Async.Start
        //interface System.IDisposable with member x.Dispose() = s:=false
        member x.TryTake(f:'a->bool) = lock v (fun() ->
            match v.FindIndex(System.Predicate f) with
            | -1 -> None
            |i ->
                let j = v.[i]
                v.RemoveAt i
                Some j
            )
        member x.Contains i = v.Contains i
        member x.Send v = sr.send v
        member x.Exists f = lock v (fun() -> v.Exists(System.Predicate f))
        member x.Take f = spinuntil(fun () -> x.Exists f);x.TryTake f|>Option.get
        //member x.Stop() = (x:>System.IDisposable).Dispose()
        member x.Start() = if not !s then async{while !s do sr.receive()|>v.Add}|>Async.Start
        member x.Receive() = lock v (fun() -> let j = v.[0] in v.RemoveAt 0; j)
        member x.SendRecieve() = {send = x.Send; receive = x.Receive}
        member x.Count = v.Count
        member x.Buffer = v
        member x.TryTakeMap (f:'a->'b option) = lock v (fun() ->
            try
                for i,j in Seq.indexed v do
                    match f j with 
                    |None -> ()
                    |k -> 
                        v.RemoveAt i
                        raise(ValueFoundException(k))
                None
            with |ValueFoundException(v) -> v|>unbox<'b option>
            )
        member x.TakeMap f = Helpers.spinuntilreturn (fun () -> x.TryTakeMap f)
    type bbsr = BufferedSendReceive<byte[]>
    let udpIP (l:IPEndPoint) (r:IPEndPoint) : bsr = 
        let s = new Sockets.Socket(l.AddressFamily,Sockets.SocketType.Dgram,Sockets.ProtocolType.Udp)
        s.Bind l
        s.Connect r
        {
            send = s.Send >> ignore//fun b -> s.SendTo(b,r)|>ignore
            receive = fun () -> let b = Array.zeroCreate<byte> 65536 in b.[..s.Receive(b)-1]
        }
    type PushPull<'a,'b> = {push:'a->'b; pull:'b->'a}
    type Comm<'a,'b> = |PushPull of PushPull<'a,'b> |SendReceive of SendReceive<'a>
    [<System.FlagsAttribute>]
    type CommType = |PushPull = 1 |SendRecieve = 2
    type bc = Comm<byte[],uint16>
