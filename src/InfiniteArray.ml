(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

type 'a t =
  { default : 'a
  ; mutable table : 'a array
      (* invariant: the length of the array [table] is nonzero *)
  }

let make n x =
  assert (n > 0);
  let default = x
  and table = Array.make n x in
  { default; table }

let rec new_length length i =
  if i < length then length else new_length (2 * length) i

let ensure a i =
  let table = a.table in
  let length = Array.length table in
  if i >= length then begin
    let table' = Array.make (new_length (2 * length) i) a.default in
    Array.blit table 0 table' 0 length;
    a.table <- table'
  end

let get a i =
  ensure a i;
  a.table.(i)

let set a i x =
  ensure a i;
  a.table.(i) <- x

module SyntacticSugar = struct
  let ( .*() ) = get

  let ( .*()<- ) = set
end
