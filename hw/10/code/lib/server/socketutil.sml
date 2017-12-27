structure SocketUtil : SOCKETUTIL =
struct

  type socket = Socket.active Socket.stream INetSock.sock

  fun connectTo (host : string, port : int) : socket option =
      (let
         val s = INetSock.TCP.socket()
         val rawaddr = NetHostDB.addr(valOf(NetHostDB.getByName(host)))
         val addr = INetSock.toAddr(rawaddr, port)
         val () = Socket.connect(s, addr)
         val () = Socket.Ctl.setKEEPALIVE(s,true)
         val () = Socket.Ctl.setOOBINLINE(s,false)
       in
         SOME(s)
       end) handle _ => NONE

  fun write (s : socket) (str : string) : unit option =
      (let
         val bts = Byte.stringToBytes str
         val len = Word8Vector.length bts
         fun send (i : int) : int =
             Socket.sendVec(s, Word8VectorSlice.slice(bts,i,NONE))
         fun put (i : int) : unit = if (i < len)
                                    then put(i + (send(i)))
                                    else ()
       in
         SOME(put 0)
       end) handle _ => NONE

  fun read (s : socket) (n : int) (timeout : Time.time option) : string option =
      let
        val {rds, wrs, exs} =
            Socket.select {rds = [Socket.sockDesc s], wrs = [], exs = [], timeout = timeout}
      in
        case (rds, wrs, exs) of
          ([desc], [], []) =>
          let
            val v = Socket.recvVec(s,n)
            val str = Byte.bytesToString(v)
          in
            case (n < 0,Word8Vector.length v = 0) of
              (true, _) => raise Size
            | (_, true) => NONE
            | _ => SOME(str)
          end
        | _ => raise Fail "Connection timed out on read"
      end

  fun close (s : socket) : unit =
      Socket.close(s)

end
