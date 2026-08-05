let random_bytes length = Mirage_crypto_rng_unix.getrandom length

let hex_of_string value =
  let alphabet = "0123456789abcdef" in
  let output = Bytes.create (String.length value * 2) in
  String.iteri
    (fun index c ->
      let code = Char.code c in
      Bytes.set output (index * 2) alphabet.[code lsr 4];
      Bytes.set output ((index * 2) + 1) alphabet.[code land 0x0f])
    value;
  Bytes.unsafe_to_string output

let random_token bytes = random_bytes bytes |> hex_of_string

let sha256 value = Digestif.SHA256.(digest_string value |> to_hex)

let password_t_cost = 3
let password_m_cost = 65_536
let password_parallelism = 1
let password_hash_len = 32
let password_salt_len = 16

let hash_password password =
  let salt = random_bytes password_salt_len in
  let encoded_len =
    Argon2.encoded_len ~t_cost:password_t_cost ~m_cost:password_m_cost
      ~parallelism:password_parallelism ~salt_len:password_salt_len
      ~hash_len:password_hash_len ~kind:Argon2.ID
  in
  match
    Argon2.ID.hash_encoded ~t_cost:password_t_cost ~m_cost:password_m_cost
      ~parallelism:password_parallelism ~pwd:password ~salt
      ~hash_len:password_hash_len ~encoded_len
  with
  | Ok encoded -> Ok (Argon2.ID.encoded_to_string encoded)
  | Error error -> Error (Argon2.ErrorCodes.message error)

let verify_password ~encoded password =
  match Argon2.verify ~encoded ~pwd:password ~kind:Argon2.ID with
  | Ok matches -> matches
  | Error _ -> false

let valid_username username =
  let length = String.length username in
  length >= 3 && length <= 32
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
         | _ -> false)
       username

let valid_password password = String.length password >= 10 && String.length password <= 256

let constant_time_equal a b =
  let length_a = String.length a and length_b = String.length b in
  let max_length = max length_a length_b in
  let difference = ref (length_a lxor length_b) in
  for index = 0 to max_length - 1 do
    let char_a = if index < length_a then Char.code a.[index] else 0 in
    let char_b = if index < length_b then Char.code b.[index] else 0 in
    difference := !difference lor (char_a lxor char_b)
  done;
  !difference = 0
