namespace C_Omega
module Logger = 
    type Logger = 
        {log : string -> unit}
        static member Printn = {log = System.Console.WriteLine}
        static member CPrintn background foreground = 
            {log = 
                (fun s -> 
                    let b, f = System.Console.BackgroundColor, System.Console.ForegroundColor
                    System.Console.BackgroundColor <- background
                    System.Console.ForegroundColor <- foreground
                    System.Console.WriteLine s
                    System.Console.BackgroundColor <- b
                    System.Console.ForegroundColor <- f
                )
            }
        static member MPrintn = {log = fun s -> lock stdout (fun() -> System.Console.WriteLine s)}
        static member MCPrintn background foreground = 
            {log = 
                (fun s -> 
                    lock stdout (fun () ->
                        let b, f = System.Console.BackgroundColor, System.Console.ForegroundColor
                        System.Console.BackgroundColor <- background
                        System.Console.ForegroundColor <- foreground
                        System.Console.WriteLine s
                        System.Console.BackgroundColor <- b
                        System.Console.ForegroundColor <- f
                    )
                )
            }
    let logf (log:Logger) pfs = Printf.kprintf log.log pfs