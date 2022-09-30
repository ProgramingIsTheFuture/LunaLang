module Error = Error

let check_types = function
  | _ :: _ ->
    assert false
  | [] -> []
