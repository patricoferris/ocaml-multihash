module M = Multihash_digestif

let multihash = Alcotest.testable M.pp M.equal
let msg ppf = function `Msg s -> Fmt.pf ppf "Error: %s" s

let err =
  let pp ppf = function
    | `Unsupported -> Fmt.pf ppf "Unsupported"
    | `Msg _ as m -> msg ppf m
  in
  Alcotest.of_pp pp

let cstruct = Alcotest.of_pp Cstruct.hexdump_pp
let v = "hello world"

let tests : (Multicodec.multihash * string) list =
  [
    ( `Sha2_256,
      "1220b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" );
    ( `Sha2_512,
      "1340309ecc489c12d6eb4cc40f50c902f2b4d0ed77ee511a7c7a9bcd3ca86d4cd86f989dd35bc5ff499670da34255b45b0cfd830e81f605dcf7dc5542e93ae9cd76f"
    );
    ( `Sha3_256,
      "1620644bcc7e564373040999aac89e7622f3ca71fba1d972fd94a31c3bfbf24e3938" );
    ( `Sha3_512,
      "1440840006653e9ac9e95117a15c915caab81662918e925de9e004f774ff82d7079a40d4d27b1b372657c61d46d470304c88c788b3a4527ad074d1dccbee5dbaa99a"
    );
    ( `Blake2b_512,
      "c0e40240021ced8799296ceca557832ab941a50b4a11f83478cf141f51f933f653ab9fbcc05a037cddbed06e309bf334942c4e58cdf1a46e237911ccd7fcf9787cbc7fd0"
    );
    ( `Blake2b_256,
      "a0e40220256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610"
    );
    (`Ripemd_160, "d3201498c615784ccb5fe5936fbc0cbe9dfdb408d92f0f");
  ]

let multihash_ident =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Multicodec.multihash_to_string v))

let test_encode_and_decode (hash, hex) () =
  (if M.is_supported hash then
   let hex' = M.of_string hash v |> Result.map M.write in
   Alcotest.(check (result cstruct err))
     "same multihash"
     (Ok (Cstruct.of_hex hex))
     hex');
  let buf = Cstruct.of_hex hex in
  let mh = M.read buf in
  let ident =
    (Result.map M.get_hash mh
      :> (Multicodec.multihash, [ `Msg of string | `Unsupported ]) result)
  in
  Alcotest.(check (result multihash_ident err))
    "same multihash_ident" (Ok hash) ident;
  if M.is_supported hash then
    let buf' = Result.map M.write mh in
    let h = Result.bind buf' M.read in
    Alcotest.(check (result multihash (Alcotest.of_pp msg)))
      "same decoding" mh h

let tests =
  let encoding =
    List.map
      (fun (ident, hex) ->
        ( Format.asprintf "encoding_decoding_%s"
            (Multicodec.multihash_to_string ident),
          `Quick,
          test_encode_and_decode (ident, hex) ))
      tests
  in
  encoding
