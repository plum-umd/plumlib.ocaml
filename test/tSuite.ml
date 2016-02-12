open Prof
open Mem
open Suite
open Implicit

let mem_suite =
  let b1  =  1
  and b42 = 42
  and b64 = 64
  and b1000 = 1000
  in
  let dbws =
    let ws = Sys.word_size in
    fun b -> b / ws
  in
  let ( >.> ) = (lsr) in
  let ( <.< ) = (lsl) in
  let show_tests =
    List.map (check_equal "Suite.Mem.show" Mem.show)
      [  bits 0, "0 bits"  ;  Bit b1, "1 bit"  ;  Bit b42, "42 bits"
      ; words 0, "0 words" ; Word b1, "1 word" ; Word b42, "42 words"
      ; bytes 0, "0 bytes" ; Byte b1, "1 byte" ; Byte b42, "42 bytes"
      ;   kib 0, "0 KiB"   ;  KiB b1, "1 KiB"  ;  KiB b42, "42 KiB"
      ;   mib 0, "0 MiB"   ;  MiB b1, "1 MiB"  ;  MiB b42, "42 MiB"
      ;   gib 0, "0 GiB"   ;  GiB b1, "1 GiB"  ;  GiB b42, "42 GiB"
      ]
  and show'_tests =
    List.map (check_equal "Basic.show" show)
      [  bits 0, "0 bits"  ;  Bit b1, "1 bit"  ;  Bit b42, "42 bits"
      ; words 0, "0 words" ; Word b1, "1 word" ; Word b42, "42 words"
      ; bytes 0, "0 bytes" ; Byte b1, "1 byte" ; Byte b42, "42 bytes"
      ;   kib 0, "0 KiB"   ;  KiB b1, "1 KiB"  ;  KiB b42, "42 KiB"
      ;   mib 0, "0 MiB"   ;  MiB b1, "1 MiB"  ;  MiB b42, "42 MiB"
      ;   gib 0, "0 GiB"   ;  GiB b1, "1 GiB"  ;  GiB b42, "42 GiB"
      ]
  and conv_tests =
    List.map (check_equal "Suite.Mem.proj_bits" Mem.proj_bits)
      [    bits 42, b42
      ; words 1000, Sys.word_size * 1000
      ;   bytes 64, b64 <.<  3
      ;     kib 64, b64 <.< 13
      ;     mib 64, b64 <.< 23
      ;     gib 64, b64 <.< 33
      ] @
    List.map (check_equal "Suite.Mem.proj_words" Mem.proj_words)
      [    bits 42, dbws b42 * 10
      ; words 1000, b1000
      ;   bytes 64, dbws (b64 <.<  3)
      ;     kib 64, dbws (b64 <.< 13)
      ;     mib 64, dbws (b64 <.< 23)
      ;     gib 64, dbws (b64 <.< 33)
      ] @
    List.map (check_equal "Suite.Mem.proj_bytes" Mem.proj_bytes)
      [    bits 42, b42 >.> 3
      ; words 1000, (b1000 >.> 3) * Sys.word_size
      ;   bytes 64, b64
      ;     kib 64, b64 <.< 10
      ;     mib 64, b64 <.< 20
      ;     gib 64, b64 <.< 30
      ] @
    List.map (check_equal "Suite.Mem.proj_kib" Mem.proj_kib)
      [    bits 42, b42 >.> 13
      ; words 1000, dbws (b1000 >.> 13)
      ;   bytes 64, b64 >.> 10
      ;     kib 64, b64
      ;     mib 64, b64 <.< 10
      ;     gib 64, b64 <.< 20
      ] @
    List.map (check_equal "Suite.Mem.proj_mib" Mem.proj_mib)
      [    bits 42, b42 >.> 23
      ; words 1000, dbws (b1000 >.> 23)
      ;   bytes 64, b64 >.> 20
      ;     kib 64, b64 >.> 10
      ;     mib 64, b64
      ;     gib 64, b64 <.< 10
      ] @
    List.map (check_equal "Suite.Mem.proj_gib" Mem.proj_gib)
      [    bits 42, b42 >.> 33
      ; words 1000, dbws (b1000 >.> 33)
      ;   bytes 64, b64 >.> 30
      ;     kib 64, b64 >.> 20
      ;     mib 64, b64 >.> 10
      ;     gib 64, b64
      ]
  and plus_tests =
    List.map (check_equal2 "Suite.Mem.(+!)" Mem.(+!))
      [ bits 42, words 2, Bit ((Sys.word_size * 2) + 42)
      ; words 2, bits 42, Bit ((Sys.word_size * 2) + 42)
      ; gib 2, mib 2048,  MiB 4096
      ; bits 1, gib 1, bits 8589934593
      ; kib    1, gib  50000000000, kib 52428800000000001
      ; kib 1024, gib 100000000000, mib   102400000000001
      ; kib    1, gib 100000000000, gib      100000000000
      ; kib    1, gib 102400000000000, gib      102400000000000
      ; (let max = gib max_int in (bits 1, max, max))
      ]
  and minus_tests =
    List.map (check_equal2 "Suite.Mem.(-!)" Mem.(-!))
      [ words 2, bits 42, Bit ((Sys.word_size * 2) - 42)
      ; bits 128, words 2, Bit 0
      ; gib 2, mib 2048,  MiB 0
      ; gib 1, bits 1, bits (1024 * 1024 * 1024 * 8 - 1)
      ; gib    50000000000, kib    1, kib 52428799999999999
      ; gib   100000000000, kib 1024, mib   102399999999999
      ; gib 10000000000000, kib    1, gib    10000000000000
      ; (let max = gib max_int in (bits 1, max, max))
      ]
  in
  "mem",
  show_tests@show'_tests@conv_tests@
  plus_tests@minus_tests

let suite = mem_suite
