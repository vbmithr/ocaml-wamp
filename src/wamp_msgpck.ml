open StdLabels

module Backend = struct
  type repr = Msgpck.t

  let rec of_repr = function
  | Msgpck.Bool b -> Wamp.Element.Bool b
  | Int i -> Int i
  | String s -> String s
  | List l -> List (List.map l ~f:of_repr)
  | Map m ->
      Dict (List.map m ~f:(function
        | Msgpck.String s, v -> s, of_repr v
        | _ -> invalid_arg "of_repr"))
  | _ -> invalid_arg "of_repr"

  let rec to_repr = function
  | Wamp.Element.Int i -> Msgpck.Int i
  | String s -> String s
  | Bool b -> Bool b
  | Dict d -> Map (List.map d ~f:(fun (k, v) -> Msgpck.String k, to_repr v))
  | List l -> List (List.map l ~f:to_repr)
end

include Wamp.Make(Backend)
