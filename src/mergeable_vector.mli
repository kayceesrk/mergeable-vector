(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Mergeable vector based on operational transformation

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

module type Vector = sig

  (** {1 Vector} *)

  type atom
  (** The type of vector element. *)

  type t
  (** The type of vector. *)

  val length : t -> int
  (** Length of vector. *)

  val set : t -> int -> atom -> t
  (** [set v i e] updates the element at position [i] in [v] to [e].

      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range 0 to [length v - 1]. *)

  val get : t -> int -> atom
  (** [get v i] returns the element at position [i] in [v].

      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range 0 to [length v - 1]. *)

  val insert : t -> int -> atom -> t
  (** [insert v i e] inserts the element [e] at position [i] in [v].

      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range 0 to [length v]. *)


  val delete : t -> int -> t
  (** [delete v i] deletes the element at position [i] in [v].

      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range 0 to [length v - 1]. *)
end

module type Mergeable_vector = sig

  (** {1 Mergeable-vector} *)

  include Vector

  type patch
  (** The type of patch. *)

  val diff : t -> t -> patch
  (** [diff a b] returns a patch [p] such that [apply a p = b]. The difference
      is computed by Wagner-Fischer algorithm. O(length(a) * length(b)) time and
      space. *)

  val apply : t -> patch -> t
  (** [apply a p] applies the compatibe patch [p] on [a].

      Raise [Invalid_argument "incompatible patch"]
      if the patch cannot be applied. *)

  val merge : resolve:(atom -> atom -> atom) -> ancestor:t -> t -> t -> t
  (** [merge r a l r] performs a 3-way merge between two vectors [l] and [r],
      and their common ancestor [a]. Merge conflicts are handled by the
      [resolve] function. *)

end

module Make (V : Vector) : Mergeable_vector with type atom = V.atom and type t = V.t

module String : sig

  (** {1 Mergeable string} *)

  include Mergeable_vector with type t = string and type atom = char
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
