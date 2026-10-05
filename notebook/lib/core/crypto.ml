(** 口令派生的对称加密。

    设计要点（对应方案里的"只加密正文"）：
    - 标题、标签、时间戳保持明文，这样 FTS5 索引和列表渲染都能直接用
    - 正文和图片字节用 AES-256-GCM 加密，每次加密换新的 12 字节 nonce
    - 口令错误或密文被篡改时，认证标签校验失败 -> 返回 Error，
      绝不会静默产出乱码正文
    - KDF 参数（算法、迭代次数、盐）存进 DB 的 crypto_meta 表，
      以后想换 Argon2id 或提高迭代次数只需加迁移，旧库仍能打开 *)

type error =
  | Bad_password  (** 认证失败：口令不对，或密文被篡改 *)
  | Crypto_error of string

exception Crypto_exn of error

let fail e = raise (Crypto_exn e)

(* KDF 参数：默认值写进 DB，日后调整只改这里 + 加迁移 *)
let kdf_algorithm = "pbkdf2-hmac-sha512"
let kdf_iterations = 600_000
let key_len = 32
let nonce_len = 12

let rng = Cryptokit.Random.secure_rng
let random_bytes n = Cryptokit.Random.string rng n

let new_salt () = random_bytes 16

let derive_key ~salt ~password =
  Cryptokit.KD.pbkdf2 Cryptokit.MAC.hmac_sha512 password salt kdf_iterations key_len

type t = { key : string }

(* 用前擦掉，避免口令派生材料在内存里留太久 *)
let wipe t = Cryptokit.wipe_string t.key

let create ~password ~salt = { key = derive_key ~salt ~password }

(** 把明文加密成 (密文含认证标签, nonce)。

    约定：nonce 不放进密文里，单独存列，方便索引和排障。 *)
let encrypt t plaintext =
  let nonce = random_bytes nonce_len in
  let cipher =
    Cryptokit.auth_transform_string
      (Cryptokit.AEAD.aes_gcm ~iv:nonce t.key Cryptokit.AEAD.Encrypt)
      plaintext
  in
  (cipher, nonce)

let decrypt t ~cipher ~nonce =
  match
    Cryptokit.auth_check_transform_string
      (Cryptokit.AEAD.aes_gcm ~iv:nonce t.key Cryptokit.AEAD.Decrypt)
      cipher
  with
  | Some plain -> plain
  | None -> fail Bad_password

(** 未加密模式下的直通"加密"，让上层代码不必写两套分支。 *)
module Plain = struct
  type t = unit

  let create ?password:_ ?salt:_ () = ()
  let wipe () = ()
  let encrypt () s = (s, "")
  let decrypt () ~cipher ~nonce:_ = cipher
end
