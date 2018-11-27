namespace ConditionCompiler

module Types =
    type Value = 
    | Bool of bool
    | Number of double
    | NumberArray of double array
    | String of string
    | StringArray of string array
    with
        static member ArrayContains (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, NumberArray na2 -> na2 |> Array.contains n1
            | String s1, StringArray sa2 -> sa2 |> Array.exists(fun q -> q.Equals(s1, System.StringComparison.OrdinalIgnoreCase))
            | _ -> failwith "Unsupported operation"

        static member StartsWith (v1:Value, v2:Value) =
            match v1, v2 with
            | String s1, String s2 -> s2.StartsWith(s1, System.StringComparison.OrdinalIgnoreCase)
            | String s1, StringArray sa2 -> sa2 |> Array.exists(fun q -> q.StartsWith(s1, System.StringComparison.OrdinalIgnoreCase))
            | _ -> failwith "Unsupported operation"

        static member EndsWith (v1:Value, v2:Value) =
            match v1, v2 with
            | String s1, String s2 -> s2.EndsWith(s1, System.StringComparison.OrdinalIgnoreCase)
            | String s1, StringArray sa2 -> sa2 |> Array.exists(fun q -> q.EndsWith(s1, System.StringComparison.OrdinalIgnoreCase))
            | _ -> failwith "Unsupported operation"

        static member Contains (v1:Value, v2:Value) =
            match v1, v2 with
            | String s1, String s2 -> s2.IndexOf(s1, System.StringComparison.OrdinalIgnoreCase) > -1
            | String s1, StringArray sa2 -> sa2 |> Array.exists(fun q -> q.IndexOf(s1, System.StringComparison.OrdinalIgnoreCase) > -1)
            | _ -> failwith "Unsupported operation"

        static member op_Equality (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, Number n2 -> n1 = n2
            | Bool b1, Bool b2 -> b1 = b2
            | String s1, String s2 -> s1.Equals(s2, System.StringComparison.OrdinalIgnoreCase)
            | _ -> failwith "Unsupported operation"

        static member op_Inequality (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, Number n2 -> n1 <> n2
            | Bool b1, Bool b2 -> b1 <> b2
            | String s1, String s2 -> not (s1.Equals(s2, System.StringComparison.OrdinalIgnoreCase))
            | String s1, StringArray sa2 -> sa2 |> Array.contains s1 |> not
            | _ -> failwith "Unsupported operation"

        static member op_LessThan (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, Number n2 -> n1 < n2
            | _ -> failwith "Unsupported operation"

        static member op_GreaterThan (v1:Value, v2:Value) = 
            match v1, v2 with
            | Number n1, Number n2 -> n1 > n2
            | _ -> failwith "Unsupported operation"
            
        static member op_LessThanOrEqual (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, Number n2 -> n1 <= n2
            | _ -> failwith "Unsupported operation"

        static member op_GreaterThanOrEqual (v1:Value, v2:Value) =
            match v1, v2 with
            | Number n1, Number n2 -> n1 >= n2
            | _ -> failwith "Unsupported operation"