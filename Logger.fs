namespace C_Omega
module Logger = 
    type Logger = 
        {log : string -> unit}
        static member Printn = {log = System.Console.WriteLine}
    let logf (log:Logger) pfs = Printf.kprintf log.log pfs