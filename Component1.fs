﻿namespace C_Omega
module Helpers = 
    ///A exception for breaking a loop
    exception ValueFoundException of obj
    type Logger = 
        {log : string -> unit}
        static member Printn = {log = System.Console.WriteLine}
    type Stopwatch() = 
        let s = new System.Diagnostics.Stopwatch()
        member x.Start() = s.Start()
        member x.Stop() = s.Stop()
        member x.Elapsed = s.ElapsedMilliseconds
        member x.Reset() = s.Reset()
        member x.HasStarted = s.IsRunning
    let justreturn v = fun _ -> v
    let enum<'a,'b when 'a : enum<'b>> : 'b->'a = LanguagePrimitives.EnumOfValue
    let deEnum<'a,'b when 'a : enum<'b>> : 'a->'b = LanguagePrimitives.EnumToValue
    let _b_uint16 b = System.BitConverter.ToUInt16(b,0)
    let _b_uint64 b = System.BitConverter.ToUInt64(b,0)
    let _uint64_b (i:uint64) = System.BitConverter.GetBytes i
    let _uint16_b (i:uint16) = System.BitConverter.GetBytes i
    let (|Empty|_|) s = if Seq.isEmpty s then Some(Empty) else None
    let (|IsIn|_|) s e = if Seq.contains e s then Some IsIn else None
    let (|IsNotIn|_|) s e = if Seq.contains e s then None else Some IsNotIn
    let (|F|_|) f v = if f v then Some(F) else None
    let r = System.Random()
    let randb n = let b = Array.zeroCreate n in r.NextBytes b;b
    let rand_int64() = r.NextDouble()|>System.BitConverter.DoubleToInt64Bits
    let _int64_b (i:int64) = System.BitConverter.GetBytes(i)
    let _b_int64 b = System.BitConverter.ToInt64(b,0)
    let _int32_b (i:int32) = System.BitConverter.GetBytes(i)
    let _b_int32 b = System.BitConverter.ToInt32(b,0)
    let loopbackv4 = System.Net.IPAddress.Loopback
    let loopbackv6 = System.Net.IPAddress.IPv6Loopback
    let ip = System.Net.IPAddress.Parse
    let anyv4 = System.Net.IPAddress.Any
    let anyv6 = System.Net.IPAddress.IPv6Any
    let ep (a:System.Net.IPAddress) p = System.Net.IPEndPoint(a,p)
    let flipc f = fun b a -> f a b
    let flipt f = function |b,a -> a,b
    let both a b v = (a v,b v)
    let side a v = a v; v
    let spinuntil f = System.Threading.SpinWait.SpinUntil(System.Func<_> f)
    let spin i = System.Threading.SpinWait.SpinUntil(System.Func<_>(fun _ -> false),(i:int))|>ignore
    let spinuntilreturn f = 
        let v = ref None
        System.Threading.SpinWait.SpinUntil(System.Func<_>(fun() -> match f() with |None -> false |v' -> v := v';true))
        v.Value.Value
    let timeoutyield f t =
        let v = ref None
        System.Threading.SpinWait.SpinUntil(System.Func<_>(fun() -> match f() with |None -> false |v' -> v := v';true),(t:int))|>ignore
        v.Value
    let shuffle(v:'a[]) = 
        let v = Array.copy v
        let f() = r.Next(0,v.Length)
        let swap a b = 
            let c = v.[a]
            v.[a] <- v.[b]
            v.[b] <- c
        for i = 0 to v.Length - 1 do f()|> swap i
        v
    let arg (f:'a->'b->'c) (v:'a) = f v
    let xor = Array.map2 (arg (^^^))
type Mapper<'a,'b> =  
    {map:'a->'b;unmap:'b->'a}
    static member Delay i = {map = (fun a -> Helpers.spin i; a); unmap = fun a -> Helpers.spin i;a}
type Verifier<'a,'b> = {sign : 'a -> 'b;verify : 'b -> 'a option}
type ev<'a> = Verifier<'a,'a>
type bv = ev<byte[]>
module Comm = 
    open System.Net
    open Helpers
    exception WrongCommType
    type SendReceive<'a> = 
        {send : 'a -> unit;receive : unit -> 'a}
        member x.Map (m:Mapper<'b,_>) = {send = m.map >> x.send;receive = x.receive >> m.unmap}
        member x.Log (logger:Logger) = {send = x.send >> side (logger.log << (sprintf ">%A")); receive = x.receive >> side (logger.log << (sprintf "<%A"))}
        member x.FlushTo (v:'a) = if Unchecked.equals v (x.receive()) then () else x.FlushTo v
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