structure Find150 :> sig val is150Book : Book.page list -> bool end =
struct

  (* is150Book : Book.page list -> bool
   * REQUIRES: true
   * ENSURES:
   *   is150Book b = true iff b contains at least three instance of
   *   the word "lambda" and no instances of the word "pointer"
   *)
   structure NoPointers = MkNemesis(Honk)
   structure PollyReset = MkCompose(
      structure StudentA = Polly
      structure StudentB = Reset
    )

    structure is150 = MkCompose(
      structure StudentA = PollyReset
      structure StudentB = NoPointers
    )

    structure R = MkReader(is150)

  fun is150Book b = R.read (Book.bind b)
end
