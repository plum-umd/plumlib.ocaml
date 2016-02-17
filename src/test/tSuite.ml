open Prof
open Mem
open Suite
open Types

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
  and of_string_tests =
    List.map (check_equal "Mem.of_string" Mem.of_string)
      [  "0 bits",  Some (bits 0)  ;  "1 bit",   Some (Bit b1)
      ; "42 bits",  Some (Bit b42) ;  "0 words", Some (words 0)
      ;  "1 word",  Some (Word b1) ; "42 words", Some (Word b42)
      ;  "0 bytes", Some (bytes 0) ;  "1 byte",  Some (Byte b1)
      ; "42 bytes", Some (Byte b42);  "0 KiB",   Some (kib 0)
      ;  "1 KiB",   Some (KiB b1)  ; "42 KiB",   Some (KiB b42)
      ;  "0 MiB",   Some (mib 0)   ;  "1 MiB",   Some (MiB b1)
      ; "42 MiB",   Some (MiB b42) ;  "0 GiB",   Some (gib 0)
      ;  "1 GiB",   Some (GiB b1)  ; "42 GiB",   Some (GiB b42)
      ;  "0bits",   Some (bits 0)  ;  "1bit",    Some (Bit b1)
      ; "42bits",   Some (Bit b42) ;  "0words",  Some (words 0)
      ;  "1word",   Some (Word b1) ; "42words",  Some (Word b42)
      ;  "0bytes",  Some (bytes 0) ;  "1byte",   Some (Byte b1)
      ; "42bytes",  Some (Byte b42);  "0KiB",    Some (kib 0)
      ;  "1KiB",    Some (KiB b1)  ; "42KiB",    Some (KiB b42)
      ;  " 1   ",   Some (KiB b1)  ; "   42",   Some (KiB b42)
      ;  "0MiB",    Some (mib 0)   ;  "1MiB",    Some (MiB b1)
      ; "42MiB",    Some (MiB b42) ;  "0GiB",    Some (gib 0)
      ;  "1GiB",    Some (GiB b1)  ; "42GiB",    Some (GiB b42)
      ;  "0b",      Some (bits 0)  ;  "1b",      Some (Bit b1)
      ; "42b",      Some (Bit b42) ;  "0w",      Some (words 0)
      ;  "1w",      Some (Word b1) ; "42w",      Some (Word b42)
      ;  "0B",      Some (bytes 0) ;  "1B",      Some (Byte b1)
      ; "42B",      Some (Byte b42);  "0K",      Some (kib 0)
      ;  "1K",      Some (KiB b1)  ; "42K",      Some (KiB b42)
      ;  "0M",      Some (mib 0)   ;  "1M",      Some (MiB b1)
      ; "42M",      Some (MiB b42) ;  "0G",      Some (gib 0)
      ;  "1G",      Some (GiB b1)  ; "42G",      Some (GiB b42)
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
  plus_tests@minus_tests@of_string_tests

let suite = mem_suite
