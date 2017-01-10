namespace C_Omega
module ArraySliceImprovement = 
    type 'a ``[]``  with member x.GetSlice(?a,?b) = let a = defaultArg a 0 in System.ArraySegment<'a>(x,a,(defaultArg b x.Length) - (a+1)).Array
open ArraySliceImprovement
module Helpers = 
    ///A exception for breaking a loop
    exception ValueFoundException of obj
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
    let XOR = Array.map2 (arg (^^^))
    let promptf pfs = Printf.kprintf (fun s -> printf "%s: " s;System.Console.ReadLine()) pfs
    let backspace() = 
        let v = System.Console.CursorLeft
        let n = System.Console.CursorLeft <- v-1
        System.Console.Write(' ')
        System.Console.CursorLeft <- v-1
    let readPassword() =
        let rec inner (s:string) = 
            let k = System.Console.ReadKey(true)
            if k.Key = System.ConsoleKey.Enter then System.Console.WriteLine(); s 
            elif k.Key = System.ConsoleKey.Backspace then 
                if s.Length > 1 then backspace();inner s.[..s.Length-2]
                elif s.Length = 1 then backspace();inner ""
                else printf "\a"; inner s
            else printf "·";inner (s+k.KeyChar.ToString())
        inner ""
    let _b_string (b:byte[]) = System.Text.Encoding.UTF8.GetString b
    let _string_b (b:string) = System.Text.Encoding.UTF8.GetBytes  b
    let (|HasFlag|_|) (flag : 'a when 'a : enum<'b>) (test : 'a) = if (test :> System.Enum).HasFlag flag then Some HasFlag else None
    let (|Is|_|) a b = if a = b then Some Is else None
    let (|IsNot|_|) a b = if a <> b then Some IsNot else None
    let try_slice_index (slice : 'a []) (target : 'a []) =
        let rec f i j =
            if j = slice.Length then i - slice.Length
            elif i = target.Length then -1
            elif target.[i] = slice.[j] then f (i + 1) (j + 1)
            else f (i + 1) 0
        let q = f 0 0
        if q = -1 then None else Some q
    let replace_all (old_slice : 'a[]) new_slice (target : 'a []) =
        let i = old_slice.Length
        let rec inner acc j =
            match try_slice_index old_slice target.[j..] with
            | None   -> Array.append acc target.[j ..]
            | Some k -> 
                let k = k + j
                let n' = k + i
                printfn "!%A-%A" k n'
                let res = Array.concat [| acc; target.[j .. k - 1]; new_slice |]
                inner res n'
        inner [||] 0
    //Exclusive. Equally as efficient at removing one as it is at removing many, and can be used instead of a.[..i] or a.[..j]
    let remove_between i j (a : _[]) = 
        let i' = (i = -1)
        let j' = (j = a.Length)
        if i' && j' then [||] elif i' then a.[j..] elif j' then a.[..i] else Array.append a.[..i] a.[j..]
type Mapper<'a,'b> =  
    {map:'a->'b;unmap:'b->'a}
    static member Delay i = {map = (fun a -> Helpers.spin i; a); unmap = fun a -> Helpers.spin i;a}
type Verifier<'a,'b> = {sign : 'a -> 'b;verify : 'b -> 'a option}
type ev<'a> = Verifier<'a,'a>
type bv = ev<byte[]>
