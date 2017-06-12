open StdLabels

module Backend = struct
  type repr = Yojson.Safe.json

  let rec of_repr = function
  | `Bool b -> Wamp.Element.Bool b
  | `Int i -> Int i
  | `String s -> String s
  | `List l -> List (List.map l ~f:of_repr)
  | `Assoc a -> Dict (List.map a ~f:(fun (s, v) -> s, of_repr v))
  | _ -> invalid_arg "of_repr"

  let rec to_repr = function
  | Wamp.Element.Int i -> `Int i
  | String s -> `String s
  | Bool b -> `Bool b
  | Dict d -> `Assoc (List.map d ~f:(fun (k, v) -> k, to_repr v))
  | List l -> `List (List.map l ~f:to_repr)
end

include Wamp.Make(Backend)
