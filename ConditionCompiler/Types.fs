namespace ConditionCompiler

module Types =
    type Value = 
    | Bool of bool
    | Number of double
    | String of string
    with
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